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
      designDB.patch(anonymizeList.map(a => a -> Some(a.anonymize)).toMap)
    }
  }

  implicit class Flatten(designDB : DFDesign.DB) {
    import designDB.getset
    private def flattenPortIn(block : DFDesign.Block, p : DFAny.Port.In[_,_]) : DFDesign.DB = {
      val refsOfPort = designDB.memberTable(p)
      val ownerMembers = designDB.ownerMemberTable(block.getOwnerDesign) //TODO: perhaps at any hierarchy?
      val connectedToPortList = ownerMembers collect {
        case m : DFNet.Connection if refsOfPort.contains(m.toRef) => designDB.refTable(m.fromRef)
      }
      assert(connectedToPortList.size == 1)
      val connectedToPort = connectedToPortList.head
      val blockMembers = designDB.ownerMemberTable(block) //TODO: perhaps at any hierarchy?
      val refPatch = blockMembers collect {
        case m : DFNet if refsOfPort.contains(m.fromRef) => m.toRef -> connectedToPort
      }
      ???
    }
    def flatten(design : DFDesign) : DFDesign.DB = {
      val block = design.block
      if (block.isTop) designDB else {
        val members = designDB.ownerMemberTable(block)
        val owner = block.getOwner
        val updatedRefTable = members.foldLeft(designDB.refTable) ((rt, m) =>
          rt.updated(m.ownerRef, owner)
        )
        val removalOrRenamePatch = (block -> None) :: members.collect {
          case m : DFAny.Port.In[_,_] => (m -> None)
          case m : DFAny.Port.Out[_,_] => (m -> None)
          case m if !m.isAnonymous => m -> Some(m.setName(s"${block.name}_${m.name}"))
        }
        designDB.patch(removalOrRenamePatch.toMap).copy(refTable = updatedRefTable)
      }
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