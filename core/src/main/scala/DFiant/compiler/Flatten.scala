package DFiant
package compiler

import DFDesign.DB.Patch

final class FlattenOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  private def flattenName(member : DFMember) : DFMember = member.setName(s"${member.getOwnerBlock.name}_${member.name}")
  private def flattenPort(port : DFAny) : List[(DFMember, Patch)] = {
    val incomingBlock = port match {
      case DFAny.In() => port.getOwnerDesign.getOwnerDesign
      case DFAny.Out() => port.getOwnerDesign
    }
    val producersToPort = designDB.consumerDependencyTable(port)
    if (producersToPort.size == 1) {
      val producerToPort = producersToPort.head
      val ibMembers = designDB.blockMemberTable(incomingBlock) //TODO: perhaps at any hierarchy?
      val unusedNet = ibMembers.collectFirst{
        case m : DFNet.Connection if m.toRef.get == port => m
      }.get
      val replacement = if (producerToPort.isAnonymous) {
        if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(port.name)
        else producerToPort
      } else producerToPort
      List((port : DFMember, Patch.Replace(replacement, Patch.Replace.Config.FullReplacement)), (unusedNet, Patch.Remove))
    } else {
      List(port -> Patch.Replace(flattenName(DFAny.Dcl(port.dfType, DFAny.Modifier.NewVar, None, port.ownerRef, port.tags)), Patch.Replace.Config.FullReplacement))
    }
  }
  private def flattenPatch(block : DFBlock) : List[(DFMember, Patch)] = {
    if (block.isTop) List() else {
      val members = designDB.blockMemberTable(block)
      val owner = block.getOwnerDesign
      (block -> Patch.Replace(owner, Patch.Replace.Config.FullReplacement)) :: members.flatMap {
        case p @ DFAny.Port.In() => flattenPort(p)
        case p @ DFAny.Port.Out() => flattenPort(p)
        case m if !m.isAnonymous => List(m -> Patch.Replace(flattenName(m), Patch.Replace.Config.FullReplacement))
        case _ => None
      }
    }
  }
  def flattenInline = {
    val inlineBlocks = designDB.members.collect{case ib@DFDesign.Block.Internal(_,_,_,Some(_)) => ib}
    val patchList = inlineBlocks.flatMap(ib => flattenPatch(ib))
    c.newStage[Flatten](designDB.patch(patchList), Seq())
  }
  def flatten(design : DFDesign*) : DFDesign.DB = designDB.patch(design.flatMap(d => flattenPatch(d.owner)).toList)
}

trait Flatten extends Compilable.Stage