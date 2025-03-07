package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable
import scala.reflect.classTag

//see `uniqueNames` for additional information
private abstract class UniqueNames(reservedNames: Set[String], caseSensitive: Boolean)
    extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
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
    // the existing design names
    val designNames = designDB.members.collect { case block: DFDesignBlock => block.dclName }
    // the existing global member names
    val globalNamedMembers = designDB.membersGlobals.filterNot(_.isAnonymous)
    // reserved names in lower case (if case-insensitive)
    val reservedNamesLC = lowerCases(reservedNames)
    // global type tagging for unique renamed names
    val globalTagList = renamer(designDB.getGlobalNamedDFTypes, reservedNamesLC)(
      _.getName,
      (e, n) => (e, classTag[NameTag]) -> NameTag(n)
    )
    // the global reserved type names, after unique global type renaming
    val globalReservedTypeNames: Set[String] =
      (designDB.getGlobalNamedDFTypes.map(e => e.getName) ++ globalTagList.map(e =>
        e._2.name
      ) ++ designNames ++ reservedNames)
    val globalReservedTypeNamesLC = lowerCases(globalReservedTypeNames)

    val localReservedNamesLCMutable = mutable.Set.from[String](reservedNamesLC)
    // global named member patching
    val globalNamedMemberPatchList = renamer(
      globalNamedMembers,
      globalReservedTypeNamesLC
    )(
      _.getName,
      (m, n) =>
        localReservedNamesLCMutable += lowerCase(n)
        m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
    ).toList
    // the reserved names for local (design) values will be the given reservedNames
    // and the now additional global member names after renaming
    val localReservedNamesLC = localReservedNamesLCMutable.toSet

    // going through all blocks with their own scope for unique names
    val blockPatchesAndTags = designDB.blockMemberList.map { case (block, members) =>
      val localTagList = block match
        case design: DFDesignBlock =>
          renamer(designDB.getLocalNamedDFTypes(design), globalReservedTypeNamesLC)(
            _.getName,
            (e, n) => (e, classTag[NameTag]) -> NameTag(n)
          )
        case _ => Nil
      val patchList = renamer(
        members.view.flatMap {
          // ignore iterator declarations that can repeat the same name wihtout collision
          // TODO: an iterator declaration may still collide with other members. Need to revisit this.
          case IteratorDcl() => None
          // no need to rename binds, since there is no collision
          // and will be handled after the binds are converted to explicit selectors
          case Bind(_)                             => None
          case m: DFMember.Named if !m.isAnonymous => Some(m)
          case _                                   => None
        },
        localReservedNamesLC
      )(
        _.getName,
        (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
      )
      (patchList, localTagList)
    }.unzip
    val patchList = globalNamedMemberPatchList ++ blockPatchesAndTags._1.flatten
    val tagList = blockPatchesAndTags._2.flatten ++ globalTagList
    designDB.patch(patchList).setGlobalTags(tagList)
  end transform
end UniqueNames

case object DFHDLUniqueNames extends UniqueNames(Set(), caseSensitive = true)

extension [T: HasDB](t: T)
  def uniqueNames(reservedNames: Set[String], caseSensitive: Boolean)(using CompilerOptions): DB =
    case object CustomUniqueNames extends UniqueNames(reservedNames, caseSensitive)
    StageRunner.run(CustomUniqueNames)(t.db)
