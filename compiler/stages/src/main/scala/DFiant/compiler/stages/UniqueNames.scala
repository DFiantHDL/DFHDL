package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.reflect.classTag

//see `uniqueNames` for additional information
private class UniqueNames(reservedNames: Set[String], caseSensitive: Boolean)(db: DB)
    extends Stage(db):
  override def transform: DB =
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
    val globalTagList = renamer(designDB.getGlobalNamedDFTypes, reservedNamesLC)(
      _.getName,
      (e, n) => (e, classTag[NameTag]) -> NameTag(n)
    )
    val globalNames: Set[String] =
      (designDB.getGlobalNamedDFTypes.map(e => e.getName) ++ globalTagList.map(e =>
        e._2.name
      ) ++ reservedNames)
    val globalNamesLC = lowerCases(globalNames)
    val patchesAndTags = designDB.blockMemberList.map { case (block, members) =>
      val localTagList = block match
        case design: DFDesignBlock =>
          renamer(designDB.getLocalNamedDFTypes(design), globalNamesLC)(
            _.getName,
            (e, n) => (e, classTag[NameTag]) -> NameTag(n)
          )
        case _ => Nil
      val patchList = renamer(
        members.view.flatMap {
          // no need to rename binds, since there is no collision
          // and will be handled after the binds are converted to explicit selectors
          case Bind(_)                             => None
          case m: DFMember.Named if !m.isAnonymous => Some(m)
          case _                                   => None
        },
        reservedNamesLC
      )(
        _.name,
        (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
      )
      (patchList, localTagList)
    }.unzip
    val patchList = patchesAndTags._1.flatten
    val tagList = patchesAndTags._2.flatten ++ globalTagList
    designDB.patch(patchList).setGlobalTags(tagList)
  end transform
end UniqueNames

extension [T: HasDB](t: T)
  def uniqueNames(reservedNames: Set[String], caseSensitive: Boolean): DB =
    new UniqueNames(reservedNames, caseSensitive)(
      t.db
    ).transform
