package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.printing.Namespacing
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

    // the FINAL (post-rename) global type names: without the dropped `t_struct_`-style
    // prefixes, type and value identifiers share one HDL namespace, so every value
    // renamer must reserve them
    var globalTypeNamesFinalLC: Set[String] = Set.empty
    // the names a PACKAGED declaration must avoid: the general defs group's names (which sit
    // alongside every package) plus the design and given reserved names — but NOT the names of
    // the other packages, which it can never be confused with
    var generalReservedNamesLC: Set[String] = Set.empty
    // the FINAL global type names per scope: a design-local packaged type must not collide with
    // the global types of its OWN package
    val scopeGlobalTypeNamesLC = mutable.LinkedHashMap.empty[Option[String], Set[String]]
    // The package a global declaration is emitted into (`None` = the general global defs file),
    // which is also its uniqueness SCOPE: every printer references a packaged declaration
    // through its package (`pkg::name` in SystemVerilog, `work.pkg.name` in VHDL,
    // `<namespace>.name` in DFHDL code), so two packages holding the same simple name can never
    // be confused at a use site. A backend WITHOUT packages has no packaged declarations to
    // scope by the time this runs: `DropPackages` folds their package names into their own and
    // clears their namespaces, leaving everything in the single `None` scope, which is the
    // across-the-board uniqueness such a backend needs.
    val topNamespace = designDB.top.dclMeta.namespace
    def typeScopeOf(dfType: NamedDFType): Option[String] =
      Namespacing.typePlacementOf(dfType, topNamespace)
    def memberScopeOf(m: DFMember): Option[String] =
      Namespacing.placementOf(m.meta.namespace, topNamespace)
    // ---- global named types + members (cross-design, computed once) ----
    // names resolve from member meta only, so any sub-DB getSet works; use the top's.
    val globalReservedTypeNamesLC: Set[String] = designDB.topDB.atGetSet {
      // the existing design (class) names — one per sub-DB
      val designNames = designDB.subDBs.values.map(_.top.dclName).toList
      // the global named types across the whole hierarchy
      val globalNamedTypes = designDB.hierGlobalNamedDFTypes.toList
      // the global named members, de-duplicated across the sub-DB closures that
      // share them by identity (member equality is effectively identity — every
      // distinct member carries unique refs)
      val globalNamedMembers = designDB.subDBs.values.iterator
        .flatMap(_.membersGlobals).filterNot(_.isAnonymous).toList.distinct
      // The uniqueness scopes, general defs group (`None`) first and the packages after it in
      // first-appearance order. The general group is uniquified first and then reserved for
      // every package group, because a package's content sits ALONGSIDE the general globals
      // rather than apart from them (a SystemVerilog package includes the global defs header,
      // a VHDL package uses the general package). Two DIFFERENT packages, on the other hand,
      // never see each other unqualified, so each starts from the same clean slate.
      val typeGroups = globalNamedTypes.groupByOrdered(typeScopeOf)
      val memberGroups = globalNamedMembers.groupByOrdered(memberScopeOf)
      val typesOfScope = typeGroups.toMap
      val membersOfScope = memberGroups.toMap
      val scopes = (None :: typeGroups.map(_._1) ::: memberGroups.map(_._1)).distinct
      val globalTypeFinalNames = mutable.ListBuffer.empty[String]
      // the general group's final names, reserved by every package group (empty while the
      // general group itself is being processed, since `scopes` leads with it)
      var generalNamesLC: Set[String] = Set.empty
      scopes.foreach { scope =>
        val scopeTypes = typesOfScope.getOrElse(scope, Nil)
        val scopeTypeUpdateMap =
          renamer(scopeTypes, reservedNamesLC ++ generalNamesLC)(_.name, (e, n) => e -> n).toMap
        typeUpdateMap ++= scopeTypeUpdateMap
        val scopeTypeFinalNames = scopeTypes.map(t => scopeTypeUpdateMap.getOrElse(t, t.name))
        globalTypeFinalNames ++= scopeTypeFinalNames
        scopeGlobalTypeNamesLC(scope) = lowerCases(scopeTypeFinalNames.toSet)
        // the names reserved for this scope's global members: its own type names (before and
        // after renaming), the design names, and the general group's names
        val memberReservedLC = lowerCases(
          (scopeTypes.map(_.name) ++ scopeTypeFinalNames ++ designNames ++ reservedNames).toSet
        ) ++ generalNamesLC
        val scopeMembers = membersOfScope.getOrElse(scope, Nil)
        val scopeMemberRenames =
          renamer(scopeMembers, memberReservedLC)(_.getName, (m, n) => m -> n).toMap
        // global named member patching
        scopeMembers.foreach { m =>
          scopeMemberRenames.get(m).foreach { n =>
            localReservedNamesLCMutable += lowerCase(n)
            memberRenamePatches(m) =
              m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
          }
        }
        if (scope.isEmpty)
          generalNamesLC = lowerCases(
            (scopeTypeFinalNames ++
              scopeMembers.map(m => scopeMemberRenames.getOrElse(m, m.getName)))
              .toSet
          )
      }
      globalTypeNamesFinalLC = lowerCases(globalTypeFinalNames.toSet)
      generalReservedNamesLC = generalNamesLC ++ lowerCases(designNames.toSet ++ reservedNames)
      // the global reserved type names, after unique global type renaming — every package's
      // types included, which is what a design-local type in the GENERAL scope must avoid
      lowerCases(
        (globalNamedTypes.map(_.name) ++ globalTypeFinalNames ++ designNames ++
          reservedNames).toSet
      )
    }
    // the reserved names for local (design) values: the given reservedNames, the
    // renamed global member names, and the (post-rename) global TYPE names (types and
    // values share one HDL identifier namespace)
    localReservedNamesLCMutable ++= globalTypeNamesFinalLC
    val localReservedNamesLC = localReservedNamesLCMutable.toSet

    // ---- per-design local members + local named types ----
    // going through all blocks (across all sub-DBs) with their own scope for unique names
    designDB.subDBs.values.foreach { sub =>
      sub.atGetSet {
        sub.blockMemberList.foreach { (block, members) =>
          // this design's local type names (post-rename): reserved for its value names
          var designLocalTypeNamesLC: Set[String] = Set.empty
          block match
            case design: DFDesignBlock =>
              // exclude types promoted to global across the hierarchy (handled above);
              // a single sub-DB may otherwise mis-classify a cross-design type as local
              val localTypes = sub.getLocalNamedDFTypes(design)
                .filterNot(designDB.hierGlobalNamedDFTypes.contains)
              // A design-local type declared in a package is still EMITTED into that package
              // (and referenced qualified), so it is uniquified in that package's scope: only
              // the general group's names and its own package's global type names are in its
              // way. A type in the general scope avoids every global type name, packaged ones
              // included, since nothing qualifies IT.
              localTypes.toList.groupByOrdered(typeScopeOf).foreach { (scope, scopeLocalTypes) =>
                val reservedLC = scope match
                  case None    => globalReservedTypeNamesLC
                  case Some(_) =>
                    generalReservedNamesLC ++ scopeGlobalTypeNamesLC.getOrElse(scope, Set.empty)
                renamer(scopeLocalTypes, reservedLC)(_.name, (e, n) => e -> n)
                  .foreach(entry => typeUpdateMap(entry._1) = entry._2)
              }
              designLocalTypeNamesLC = lowerCases(
                localTypes.map(t => typeUpdateMap.getOrElse(t, t.name)).toSet
              )
            case _ =>
          end match
          renamer(
            members.view.flatMap {
              // ignore iterator declarations that can repeat the same name wihtout collision
              // TODO: an iterator declaration may still collide with other members. Need to revisit this.
              case IteratorDcl() => None
              // no need to rename binds, since there is no collision
              // and will be handled after the binds are converted to explicit selectors
              case Bind(_) => None
              // design block names are their declaration names (design/class name), so they are handled differently
              case _: DFDesignBlock => None
              // text output names are statement labels, scoped to the design (see below)
              case _: TextOut                          => None
              case m: DFMember.Named if !m.isAnonymous => Some(m)
              case _                                   => None
            },
            localReservedNamesLC ++ designLocalTypeNamesLC
          )(
            _.getName,
            (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
          ).foreach(entry => memberRenamePatches(entry._1) = entry)
          // A text output's name becomes a statement label in the generated HDL, and a label
          // lives in the enclosing module/architecture namespace rather than in the process
          // that holds the statement. Labels are therefore uniquified against every name in the
          // design, in a pass of their own so that a collision renames the label and never the
          // declaration it collided with.
          block match
            case design: DFDesignBlock =>
              val designNamesLC = lowerCases(
                sub.membersNoGlobals.view.collect {
                  case m: DFMember.Named if !m.isAnonymous && !m.isInstanceOf[TextOut] =>
                    m.getName
                }.toSet
              )
              renamer(
                sub.membersNoGlobals.collect {
                  case t: TextOut if !t.isAnonymous => t
                },
                designNamesLC ++ localReservedNamesLC
              )(
                _.getName,
                (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
              ).foreach(entry => memberRenamePatches(entry._1) = entry)
            case _ =>
          end match
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
