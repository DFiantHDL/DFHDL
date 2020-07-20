package DFiant
package compiler

import DFDesign.DB.Patch

import scala.reflect.classTag

final class UniqueNamesOps[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.fixAnonymous.db
  import designDB.__getset
  def uniqueNames(reservedNames : Set[String], caseSensitive : Boolean) = {
    def lowerCase(name : String) : String = if (caseSensitive) name else name.toLowerCase
    def lowerCases(names : Set[String]) : Set[String] = if (caseSensitive) names else names.map(_.toLowerCase)
    def renamer[T, R](iter : Iterable[T], existingNamesLC : Set[String])
      (nameAccess : T => String, updateFunc : (T, String) => R)
    : Iterable[R] = {
      iter.groupBy(e => lowerCase(nameAccess(e))).flatMap {
        case (name, list) if list.size > 1 || existingNamesLC.contains(name) => list.zipWithIndex.map{case (renamed, i) =>
          val suffix = s"%0${list.size.toString.length}d".format(i)
          val updatedName = s"${nameAccess(renamed)}_$suffix"
          updateFunc(renamed, updatedName)
        }
        case _ => Nil
      }
    }

    val reservedNamesLC = lowerCases(reservedNames)
    val globalTagList = renamer(designDB.getGlobalEnumTypes, reservedNamesLC)(
      _.name,
      (e, n) => (e, classTag[EnumType.NameTag]) -> EnumType.NameTag(n)
    )
    val globalNames : Set[String] =
      (designDB.getGlobalEnumTypes.map(e => e.name) ++ globalTagList.map(e => e._2.name) ++ reservedNames)
    val globalNamesLC = lowerCases(globalNames)
    val patchesAndTags = designDB.designMemberList.map {case (design, members) =>
      val localTagList = renamer(designDB.getLocalEnumTypes(design), globalNamesLC)(
        _.name,
        (e, n) => (e, classTag[EnumType.NameTag]) -> EnumType.NameTag(n)
      )
      val patchList = renamer(members.filterNot(_.isAnonymous), globalNamesLC ++ localTagList.map(e => lowerCase(e._2.name)))(
        _.name,
        (m, n) => m -> Patch.Replace(m.setName(n), Patch.Replace.Config.FullReplacement)
      )
      (patchList, localTagList)
    }.unzip
    val patchList = patchesAndTags._1.flatten
    val tagList = patchesAndTags._2.flatten ++ globalTagList
    c.newStage(designDB.patch(patchList).setGlobalTags(tagList))
  }
}