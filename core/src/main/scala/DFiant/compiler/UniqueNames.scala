package DFiant
package compiler

import DFDesign.DB.Patch

final class UniqueNamesOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB = c.fixAnonymous.db
  import designDB.__getset
  def uniqueNames(reservedNames : Set[String], caseSensitive : Boolean) = {
    val reservedNamesLC = if (caseSensitive) reservedNames else reservedNames.map(n => n.toLowerCase)
    val patchList = designDB.designMemberList.flatMap {
      case (_, members) => members.filterNot(_.isAnonymous).groupBy(m => if (caseSensitive) m.name else m.name.toLowerCase).flatMap {
        case (name, list) if list.size > 1 || reservedNamesLC.contains(name) => list.zipWithIndex.map{case (member, i) =>
          val suffix = s"%0${list.size.toString.length}d".format(i)
          val updatedName = s"${member.name}_$suffix"
          member -> Patch.Replace(member.setName(updatedName), Patch.Replace.Config.FullReplacement)
        }
        case _ => Nil
      }
    }
    c.newStage[UniqueNames](designDB.patch(patchList))
  }
}

trait UniqueNames extends Compilation.Stage