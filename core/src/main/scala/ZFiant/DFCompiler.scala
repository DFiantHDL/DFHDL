/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package ZFiant
import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec

object DFCompiler {
  val delim : String = "  "
  implicit class Discovery(designDB : DFDesign.DB) {
    def discovery : DFDesign.DB = {
      ???
    }
  }

  implicit class Naming(designDB : DFDesign.DB) {
    import designDB.getset
    def fixNames : DFDesign.DB = {
      val anonymizeList = designDB.ownerMemberList.flatMap {
        case (block, members) => members.groupBy(m => m.tags.meta.namePosition).flatMap {
          //In case an anonymous member got a name from its owner. For example:
          //val ret = DFBits(8).ifdf(cond) {
          //  i & i
          //}
          //The `i & i` function would also get the `ret` name just as the if block itself
          case (pos, gm) if (pos == block.tags.meta.namePosition) => gm
          //In case an anonymous member was used as an argument to an owner. For example:
          //val ret = DFBits(8).ifdf(i & i) {
          //}
          //The `i & i` function would also get the `ret` name just as the if block itself
          case (_, gm) if (gm.length > 1) =>
            if (gm.collectFirst{case x : DFBlock => x}.isDefined)
              gm.collect {case a : DFAny.CanBeAnonymous => a}
            else List()
          case _ => List()
        }
      }
      designDB.patch(anonymizeList.map(a => a -> Patch.Replace(a.anonymize, Patch.Replace.Config.FullReplacement)))
    }
    @tailrec private def mcf(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
      remaining match {
        case (block : DFBlock) :: mList =>
          val members = designDB.ownerMemberTable(block)
          val sortedMembers = block match {
            case _ : DFDesign.Block =>
              val split = members.partition {
                case _ : DFDesign.Block => true
                case m : DFAny if m.modifier.isInstanceOf[DFAny.Modifier.Connectable] => true
                case _ => false
              }
              split._1 ++ split._2
            case _ => members
          }
          mcf(sortedMembers ++ mList, retList :+ block)
        case m :: mList => mcf(mList, retList :+ m)
        case Nil => retList
      }
    def moveConnectableFirst : DFDesign.DB = designDB.copy(members = mcf(List(designDB.top), List()))
  }

  implicit class Flatten(designDB : DFDesign.DB) {
    import designDB.getset
    private def flattenName(member : DFMember) : DFMember = member.setName(s"${member.getOwner.name}_${member.name}")
    private def flattenPort(port : DFAny) : List[(DFMember, Patch)] = {
      val incomingBlock = port match {
        case DFAny.In() => port.getOwnerDesign.getOwnerDesign
        case DFAny.Out() => port.getOwnerDesign
      }
      val producersToPort = designDB.consumerDependencyTable(port)
      if (producersToPort.size == 1) {
        val producerToPort = producersToPort.head
        val ibMembers = designDB.ownerMemberTable(incomingBlock) //TODO: perhaps at any hierarchy?
        val unusedNet = ibMembers.collectFirst{
          case m : DFNet.Connection if m.toRef.get == port => m
        }.get
        val replacement = if (producerToPort.isAnonymous) {
          if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(port.name)
          else producerToPort
        } else producerToPort
        List((port : DFMember, Patch.Replace(replacement, Patch.Replace.Config.FullReplacement)), (unusedNet, Patch.Remove))
      } else {
        List(port -> Patch.Replace(flattenName(DFAny.NewVar(port.dfType, DFAny.NewVar.Uninitialized, port.ownerRef, port.tags)), Patch.Replace.Config.FullReplacement))
      }
    }
    private def flattenPatch(design : DFDesign) : List[(DFMember, Patch)] = {
      val block = design.block
      if (block.isTop) List() else {
        val members = designDB.ownerMemberTable(block)
        val owner = block.getOwnerDesign
        (block -> Patch.Replace(owner, Patch.Replace.Config.FullReplacement)) :: members.flatMap {
          case p : DFAny.Port.In[_,_] => flattenPort(p)
          case p : DFAny.Port.Out[_,_] => flattenPort(p)
          case m if !m.isAnonymous => List(m -> Patch.Replace(flattenName(m), Patch.Replace.Config.FullReplacement))
          case _ => None
        }
      }
    }
    def flatten(design : DFDesign*) : DFDesign.DB = designDB.patch(design.flatMap(d => flattenPatch(d)).toList)
  }

  implicit class AddGuard(designDB : DFDesign.DB) {
    import designDB.getset
    def addIfGuard(guardMembers : Seq[DFMember], guardRefs : Seq[DFMember.Ref[_]], fromMember : DFMember, toMember : DFMember) : DFDesign.DB = {
      val block = fromMember.getOwner
      val members = designDB.ownerMemberTable(block)
      val owner = block.getOwnerDesign
      val temp = new DFDesign() {
        val myCounter = DFUInt(8) init 0
        myCounter := myCounter + 1
      }
//      val ifBlock = ConditionalBlock.NoRetVal.IfBlock()
//      (fromMember -> Patch.AddBefore(guardMembers, guardRefs))
//      (block -> Patch.ReplaceWith(owner)) :: members.flatMap {
//        case p : DFAny.Port.In[_,_] => flattenPort(p)
//        case p : DFAny.Port.Out[_,_] => flattenPort(p)
//        case m if !m.isAnonymous => List(m -> Patch.ReplaceWith(flattenName(m)))
//        case _ => None
//      }
//      designDB.patch(design.flatMap(d => flattenPatch(d)).toList)
      ???
    }
  }

  final implicit class CodeString(designDB : DFDesign.DB) {
    import designDB.getset
    def blockBodyCodeString(block : DFBlock, members : List[DFMember]) : String = {
      val membersCodeString = members.collect {
        case mh : ConditionalBlock.MatchHeader => mh.codeString
        case cb : ConditionalBlock => cb.codeString(blockBodyCodeString(cb, designDB.ownerMemberTable(cb)))
        case m : DFDesign.Block => s"final val ${m.name} = new ${m.typeName} {}" //TODO: fix
        case n : DFNet => n.codeString
        case a : DFAny if !a.isAnonymous => s"final val ${a.name} = ${a.codeString}"
      }
      membersCodeString.mkString("\n")
    }
    def codeString : String = {
      val bodyDB = new DSLOwnerConstruct.DB[DFBlock, String]{
        override def ownerToString(ownerTypeName: String, ownerBody: String): String =
          s"trait $ownerTypeName extends DFDesign {\n${ownerBody.delimRowsBy(delim)}\n}"
      }
      designDB.ownerMemberList.foreach {
        case (block : DFDesign.Block, members) =>
          bodyDB.addOwnerBody(block.typeName, blockBodyCodeString(block, members), block)
        case _ =>
      }
      bodyDB.dbString
    }
    def printCodeString() : DFDesign.DB = {
      println(codeString)
      designDB
    }
  }
}