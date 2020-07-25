package DFiant
package compiler

import DFDesign.DB.Patch

import scala.reflect.classTag

final class AddTags[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def addTags(tags : TagsOf[D]) : IRCompilation[D] = {
    val patchList = tags.attachDesign(c.dsn).getTagMap.toList.map {
      case (member, customTagMap) =>
        member -> Patch.Replace(member !! customTagMap, Patch.Replace.Config.FullReplacement)
    }
    c.newStage(designDB.patch(patchList))
  }
}