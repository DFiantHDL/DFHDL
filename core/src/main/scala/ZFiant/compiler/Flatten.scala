package ZFiant
package compiler

import DFDesign.DB.Patch

//final class Flatten[C](c : C)(implicit comp : Compilable[C]) {
//  private val designDB = comp(c)
//  import designDB.getset
//  private def flattenName(member : DFMember) : DFMember = member.setName(s"${member.getOwner.name}_${member.name}")
//  private def flattenPort(port : DFAny) : List[(DFMember, Patch)] = {
//    val incomingBlock = port match {
//      case DFAny.In() => port.getOwnerDesign.getOwnerDesign
//      case DFAny.Out() => port.getOwnerDesign
//    }
//    val producersToPort = designDB.consumerDependencyTable(port)
//    if (producersToPort.size == 1) {
//      val producerToPort = producersToPort.head
//      val ibMembers = designDB.ownerMemberTable(incomingBlock) //TODO: perhaps at any hierarchy?
//      val unusedNet = ibMembers.collectFirst{
//        case m : DFNet.Connection if m.toRef.get == port => m
//      }.get
//      val replacement = if (producerToPort.isAnonymous) {
//        if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(port.name)
//        else producerToPort
//      } else producerToPort
//      List((port : DFMember, Patch.Replace(replacement, Patch.Replace.Config.FullReplacement)), (unusedNet, Patch.Remove))
//    } else {
//      List(port -> Patch.Replace(flattenName(DFAny.NewVar(port.dfType, DFAny.NewVar.Uninitialized, port.ownerRef, port.tags)), Patch.Replace.Config.FullReplacement))
//    }
//  }
//  private def flattenPatch(block : DFBlock) : List[(DFMember, Patch)] = {
//    if (block.isTop) List() else {
//      val members = designDB.ownerMemberTable(block)
//      val owner = block.getOwnerDesign
//      (block -> Patch.Replace(owner, Patch.Replace.Config.FullReplacement)) :: members.flatMap {
//        case p : DFAny.Port.In[_,_] => flattenPort(p)
//        case p : DFAny.Port.Out[_,_] => flattenPort(p)
//        case m if !m.isAnonymous => List(m -> Patch.Replace(flattenName(m), Patch.Replace.Config.FullReplacement))
//        case _ => None
//      }
//    }
//  }
//  def flattenInline : DFDesign.DB = {
//    val inlineBlocks = designDB.members.collect{case ib@DFDesign.Block.Internal(_,_,Some(_)) => ib}
//    val patchList = inlineBlocks.flatMap(ib => flattenPatch(ib))
//    designDB.patch(patchList)
//  }
//  def flatten(design : DFDesign*) : DFDesign.DB = designDB.patch(design.flatMap(d => flattenPatch(d.block)).toList)
//}
