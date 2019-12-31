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
      designDB.patch(anonymizeList.map(a => a -> Patch.ReplaceWith(a.anonymize)))
    }
  }

  implicit class Flatten(designDB : DFDesign.DB) {
    import designDB.getset
    private def flattenPortIn(block : DFDesign.Block, p : DFAny.Port.In[_,_]) : List[(DFMember, Patch)] = {
      val producersToPort = designDB.consumerDependencyTable(p)
      assert(producersToPort.size == 1) //currently assuming there is always one an only one connected input. TODO: fix for empty
      val producerToPort = producersToPort.head
      val ownerMembers = designDB.ownerMemberTable(block.getOwnerDesign) //TODO: perhaps at any hierarchy?
      val unusedNet = ownerMembers.collectFirst{
        case m : DFNet.Connection if m.toRef.get == p => m
      }.get
      val replacement = if (producerToPort.isAnonymous) {
        if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(p.name)
        else producerToPort
      } else producerToPort
      List((p : DFMember, Patch.ReplaceWith(replacement)), (unusedNet, Patch.Remove))
    }
    private def flattenPortOut(block : DFDesign.Block, p : DFAny.Port.Out[_ <: DFAny.Type,_ <: DFAny.Modifier.Port.Out]) : List[(DFMember, Patch)] = {
      val producersToPort = designDB.consumerDependencyTable(p)
      if (producersToPort.size == 1) {
        val producerToPort = producersToPort.head
        val ownerMembers = designDB.ownerMemberTable(block.getOwnerDesign) //TODO: perhaps at any hierarchy?
        val unusedNet = ownerMembers.collectFirst{
          case m : DFNet.Connection if m.fromRef.get == p => m
        }.get
        val replacement = if (producerToPort.isAnonymous) {
          if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(p.name)
          else producerToPort
        } else producerToPort
        List((p : DFMember, Patch.ReplaceWith(replacement)), (unusedNet, Patch.Remove))
      } else {
        List(p -> Patch.ReplaceWith(DFAny.NewVar(p.dfType, DFAny.NewVar.Uninitialized, p.ownerRef, p.tags).setName(s"${block.name}_${p.name}")))
      }
    }
    private def flattenPatch(design : DFDesign) : List[(DFMember, Patch)] = {
      val block = design.block
      if (block.isTop) List() else {
        val members = designDB.ownerMemberTable(block)
        val owner = block.getOwnerDesign
        (block -> Patch.ReplaceWith(owner)) :: members.flatMap {
          case p : DFAny.Port.In[_,_] => flattenPortIn(block, p)
          case p : DFAny.Port.Out[_,_] => flattenPortOut(block, p)
          case m if !m.isAnonymous => List(m -> Patch.ReplaceWith(m.setName(s"${block.name}_${m.name}")))
          case _ => None
        }
      }
    }
    def flatten(design : DFDesign*) : DFDesign.DB = designDB.patch(design.flatMap(d => flattenPatch(d)).toList)
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