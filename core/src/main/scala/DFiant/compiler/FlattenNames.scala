package DFiant
package compiler

import DFDesign.DB.Patch
import scala.annotation.tailrec

final class FlattenNamesOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def recursiveNameFlatten(member : DFMember, name : String) : String = member.getOwner match {
    case o : DFOwner.NameFlattenOwner =>
      o.nameFlatten match {
        case DFOwner.NameFlatten.IgnoreOwnerName => recursiveNameFlatten(o, name)
        case _ => recursiveNameFlatten(o, o.nameFlatten(name, o.name))
      }
    case _ => name
  }
  private def recursiveNameFlatten(member : DFMember) : String = recursiveNameFlatten(member, member.name)
  def flattenNames = {
    val patchList = designDB.members.flatMap {
      case _ : DFDesign.Block.Top => None
      case o : DFOwner.NameFlattenOwner => Some(o -> Patch.Replace(o.getOwnerBlock, Patch.Replace.Config.ChangeRefAndRemove))
      case m if !m.isAnonymous => m.getOwner match {
        case _ : DFOwner.NameFlattenOwner =>
          val updatedName = recursiveNameFlatten(m)
          if (m.name == updatedName) None
          else Some(m -> Patch.Replace(m.setName(updatedName), Patch.Replace.Config.FullReplacement))
        case _ => None
      }
      case _ => None
    }
    c.newStage[FlattenNames](designDB.patch(patchList))
  }
}

trait FlattenNames extends Compilation.Stage