package DFiant
package compiler

import DFDesign.DB.Patch

final class RemovePureInterfacesOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  def fixName(member : DFMember) : String = member.getOwner match {
    case o : DFInterface.Owner => s"${fixName(o)}_${member.name}"
    case _ => member.name
  }
  def removePureInterfaces = {
    val patchList = designDB.members.flatMap {
      case _ : DFDesign.Block.Top => None
      case o : DFInterface.Owner => Some(o -> Patch.Replace(o.getOwnerDesign, Patch.Replace.Config.ChangeRefAndRemove))
      case m => m.getOwner match {
        case _ : DFInterface.Owner => Some(m -> Patch.Replace(m.setName(fixName(m)), Patch.Replace.Config.FullReplacement))
        case _ => None
      }
    }
    c.newStage[RemovePureInterfaces](designDB.patch(patchList), Seq())
  }
}

trait RemovePureInterfaces extends Compilable.Stage