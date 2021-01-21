package DFiant
package compiler

import DFDesign.DB.Patch

import scala.reflect.classTag

final class UniqueNames[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.fixAnonymous.db
  import designDB.__getset
  def uniqueNames(
      reservedNames: Set[String],
      caseSensitive: Boolean
  ): IRCompilation[D] = {
    def lowerCase(name: String): String =
      if (caseSensitive) name else name.toLowerCase
    def lowerCases(names: Set[String]): Set[String] =
      if (caseSensitive) names else names.map(_.toLowerCase)
    def renamer[T, R](
        iter: Iterable[T],
        existingNamesLC: Set[String]
    )(nameAccess: T => String, updateFunc: (T, String) => R): Iterable[R] = {
      iter.groupBy(e => lowerCase(nameAccess(e))).flatMap {
        case (name, list) if list.size > 1 || existingNamesLC.contains(name) =>
          list.zipWithIndex.map {
            case (renamed, i) =>
              val suffix      = s"%0${list.size.toString.length}d".format(i)
              val updatedName = s"${nameAccess(renamed)}_$suffix"
              updateFunc(renamed, updatedName)
          }
        case _ => Nil
      }
    }

    val reservedNamesLC = lowerCases(reservedNames)
    val globalTagList = renamer(designDB.getGlobalEnumEntries, reservedNamesLC)(
      _.name,
      (e, n) => (e, classTag[DFMember.NameTag]) -> DFMember.NameTag(n)
    )
    val globalNames : Set[String] =
      (designDB.getGlobalEnumEntries.map(e => e.name) ++ globalTagList.map(e => e._2.name) ++ reservedNames)
    val globalNamesLC = lowerCases(globalNames)
    val patchesAndTags = designDB.designMemberList.map {case (design, members) =>
      val localTagList = renamer(designDB.getLocalEnumEntries(design), globalNamesLC)(
        _.name,
        (e, n) => (e, classTag[DFMember.NameTag]) -> DFMember.NameTag(n)
      )
      val patchList = renamer(members.filterNot(_.isAnonymous), reservedNamesLC)(
        _.name,
        (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
      )
      (patchList, localTagList)
    }.unzip
    val patchList = patchesAndTags._1.flatten
    val tagList   = patchesAndTags._2.flatten ++ globalTagList
    c.newStage(designDB.patch(patchList).setGlobalTags(tagList))
  }
}
