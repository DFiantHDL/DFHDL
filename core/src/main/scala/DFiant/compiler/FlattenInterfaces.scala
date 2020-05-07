package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.DFInterface.NameFlatten

import scala.annotation.tailrec

final class FlattenInterfacesOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def recursiveNameFlatten(member : DFMember, name : String) : String = member.getOwner match {
    case o : DFInterface.Owner =>
      o.nameFlatten match {
        case NameFlatten.IgnoreOwnerName => recursiveNameFlatten(o, name)
        case _ => recursiveNameFlatten(o, o.nameFlatten(name, o.name))
      }
    case _ => name
  }
  private def recursiveNameFlatten(member : DFMember) : String = recursiveNameFlatten(member, member.name)
  def flattenInterfaces = {
    val patchList = designDB.members.flatMap {
      case _ : DFDesign.Block.Top => None
      case o : DFInterface.Owner => Some(o -> Patch.Replace(o.getOwnerDesign, Patch.Replace.Config.ChangeRefAndRemove))
      case m if !m.isAnonymous => m.getOwner match {
        case _ : DFInterface.Owner => Some(m -> Patch.Replace(m.setName(recursiveNameFlatten(m)), Patch.Replace.Config.FullReplacement))
        case _ => None
      }
      case _ => None
    }
    c.newStage[FlattenInterfaces](designDB.patch(patchList), Seq())
  }
}

trait FlattenInterfaces extends Compilable.Stage