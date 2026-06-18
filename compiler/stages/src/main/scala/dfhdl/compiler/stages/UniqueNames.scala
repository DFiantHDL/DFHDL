package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable
import scala.collection.immutable.ListMap

//see `uniqueNames` for additional information
private abstract class UniqueNames(reservedNames: Set[String], caseSensitive: Boolean)
    extends GlobalStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transformGlobal(designDB: DB)(using co: CompilerOptions, refGen: RefGen): DB =
    // conditionally lower cases the name according to the case sensitivity as
    // set by `caseSensitive`
    def lowerCase(name: String): String = if (caseSensitive) name else name.toLowerCase
    // the same as lowerCase but for a Set
    def lowerCases(names: Set[String]): Set[String] =
      if (caseSensitive) names else names.map(_.toLowerCase)
    // Generates an iterable of modifications required to have unique for the given
    // `iter` collection. The `existingNamesLC` provides additional context of
    // existing names (already lower-cased in case of no case sensitivity).
    def renamer[T, R](
        iter: Iterable[T],
        existingNamesLC: Set[String]
    )(nameAccess: T => String, updateFunc: (T, String) => R): Iterable[R] =
      iter.groupBy(e => lowerCase(nameAccess(e))).flatMap {
        case (name, list) if list.size > 1 || existingNamesLC.contains(name) =>
          list.zipWithIndex.map { case (renamed, i) =>
            val updatedName = s"${nameAccess(renamed)}_${i.toPaddedString(list.size)}"
            updateFunc(renamed, updatedName)
          }
        case _ => Nil
      }

    val reservedNamesLC = lowerCases(reservedNames)
    // member renames keyed by the member, BUILT ONCE so the renamed-member object
    // is shared: a global member living (by identity) in several sub-DB closures
    // is renamed identically in each and `newToOld` dedups it to one (building a
    // fresh rename per sub-DB would emit duplicate, divergent globals).
    val memberRenamePatches = mutable.LinkedHashMap.empty[DFMember, (DFMember, Patch)]
    // named-type renames (global + per-design local), consumed by the phase-2
    // dfType rewrite.
    val typeUpdateMap = mutable.LinkedHashMap.empty[NamedDFType, String]
    val localReservedNamesLCMutable = mutable.Set.from[String](reservedNamesLC)

    // ---- global named types + members (cross-design, computed once) ----
    // names resolve from member meta only, so any sub-DB getSet works; use the top's.
    val globalReservedTypeNamesLC: Set[String] = designDB.topDB.atGetSet {
      // the existing design (class) names — one per sub-DB
      val designNames = designDB.subDBs.values.map(_.top.dclName)
      // the global named types across the whole hierarchy
      val globalNamedTypes = designDB.hierGlobalNamedDFTypes
      // the global named members, de-duplicated across the sub-DB closures that
      // share them by identity (member equality is effectively identity — every
      // distinct member carries unique refs)
      val globalNamedMembers = designDB.subDBs.values.iterator
        .flatMap(_.membersGlobals).filterNot(_.isAnonymous).toList.distinct
      // global type map for unique renamed names
      val globalTypeUpdateMap =
        renamer(globalNamedTypes, reservedNamesLC)(_.name, (e, n) => e -> n).toMap
      typeUpdateMap ++= globalTypeUpdateMap
      // the global reserved type names, after unique global type renaming
      val globalReservedTypeNames: Set[String] =
        (globalNamedTypes.map(e => e.name) ++ globalTypeUpdateMap.values ++ designNames ++
          reservedNames).toSet
      val resultLC = lowerCases(globalReservedTypeNames)
      // global named member patching
      renamer(globalNamedMembers, resultLC)(
        _.getName,
        (m, n) =>
          localReservedNamesLCMutable += lowerCase(n)
          m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
      ).foreach(entry => memberRenamePatches(entry._1) = entry)
      resultLC
    }
    // the reserved names for local (design) values will be the given reservedNames
    // and the now additional global member names after renaming
    val localReservedNamesLC = localReservedNamesLCMutable.toSet

    // ---- per-design local members + local named types ----
    // going through all blocks (across all sub-DBs) with their own scope for unique names
    designDB.subDBs.values.foreach { sub =>
      sub.atGetSet {
        sub.blockMemberList.foreach { (block, members) =>
          block match
            case design: DFDesignBlock =>
              // exclude types promoted to global across the hierarchy (handled above);
              // a single sub-DB may otherwise mis-classify a cross-design type as local
              renamer(
                sub.getLocalNamedDFTypes(design)
                  .filterNot(designDB.hierGlobalNamedDFTypes.contains),
                globalReservedTypeNamesLC
              )(_.name, (e, n) => e -> n)
                .foreach(entry => typeUpdateMap(entry._1) = entry._2)
            case _ =>
          renamer(
            members.view.flatMap {
              // ignore iterator declarations that can repeat the same name wihtout collision
              // TODO: an iterator declaration may still collide with other members. Need to revisit this.
              case IteratorDcl() => None
              // no need to rename binds, since there is no collision
              // and will be handled after the binds are converted to explicit selectors
              case Bind(_) => None
              // design block names are their declaration names (design/class name), so they are handled differently
              case _: DFDesignBlock                    => None
              case m: DFMember.Named if !m.isAnonymous => Some(m)
              case _                                   => None
            },
            localReservedNamesLC
          )(
            _.getName,
            (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
          ).foreach(entry => memberRenamePatches(entry._1) = entry)
        }
      }
    }

    // ---- phase 1: patch the member names, per sub-DB ----
    val firstStepSubs: ListMap[StaticRef, DB] = ListMap.from(
      designDB.subDBs.iterator.map { (key, sub) =>
        val patches = sub.members.collect {
          case m if memberRenamePatches.contains(m) => memberRenamePatches(m)
        }
        key -> sub.patch(patches)
      }
    )
    val firstStep = designDB.update(subDBs = firstStepSubs)

    // ---- phase 2: patch the members with updated named types ----
    if (typeUpdateMap.isEmpty) firstStep
    else
      val typeUpdates = typeUpdateMap.toMap
      // built once (type rewriting reads only type structure + renames by name),
      // keyed by member so a shared global member's update is reused across every
      // sub-DB that holds it.
      val typeUpdatePatches: mutable.LinkedHashMap[DFMember, (DFMember, Patch)] =
        firstStep.topDB.atGetSet {
          object ComposedNamedDFTypeReplacement
              extends ComposedDFTypeReplacement(
                preCheck = {
                  case dt: NamedDFType => typeUpdates.get(dt)
                  case _               => None
                },
                updateFunc = { case (dt: NamedDFType, name) => dt.updateName(name) }
              )
          val patches = mutable.LinkedHashMap.empty[DFMember, (DFMember, Patch)]
          firstStep.subDBs.values.foreach { sub =>
            sub.members.foreach {
              case dfVal: DFVal =>
                dfVal.dfType match
                  case ComposedNamedDFTypeReplacement(updatedDFType) =>
                    patches.getOrElseUpdate(
                      dfVal,
                      dfVal -> Patch.Replace(
                        dfVal.updateDFType(updatedDFType),
                        Patch.Replace.Config.FullReplacement
                      )
                    )
                  case _ =>
              case _ =>
            }
          }
          patches
        }
      val secondStepSubs: ListMap[StaticRef, DB] = ListMap.from(
        firstStep.subDBs.iterator.map { (key, sub) =>
          val patches = sub.members.collect {
            case m if typeUpdatePatches.contains(m) => typeUpdatePatches(m)
          }
          key -> sub.patch(patches)
        }
      )
      firstStep.update(subDBs = secondStepSubs)
    end if
  end transformGlobal
end UniqueNames

case object DFHDLUniqueNames extends UniqueNames(Set(), caseSensitive = true)

extension [T: HasDB](t: T)
  def uniqueNames(reservedNames: Set[String], caseSensitive: Boolean)(using CompilerOptions): DB =
    case object CustomUniqueNames extends UniqueNames(reservedNames, caseSensitive)
    StageRunner.run(CustomUniqueNames)(t.db)
