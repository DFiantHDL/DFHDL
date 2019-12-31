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
      val refsOfPort = designDB.memberTable(p)
      val toRefs = refsOfPort.flatMap {
        case r : DFNet.ToRef => Some(r)
        case _ => None
      }
      assert(toRefs.size == 1) //currently assuming there is always one an only one connected input. TODO: fix for empty
      val aliasedRefs = refsOfPort.flatMap {
        case a : DFAny.Alias.RelValRef[_] => Some(a)
        case _ => None
      }
      assert(aliasedRefs.isEmpty)//TODO:fix for aliased connection
      val toRef = toRefs.head
      val ownerMembers = designDB.ownerMemberTable(block.getOwnerDesign) //TODO: perhaps at any hierarchy?
      val (connectedToPort, unusedNet) = ownerMembers.collectFirst{
        case m : DFNet.Connection if m.toRef == toRef => (designDB.refTable(m.fromRef), m)
      }.get
      println(s"removing unused net ${unusedNet.codeString}")
      List((p : DFMember, Patch.ReplaceWith(connectedToPort)), (unusedNet, Patch.Remove))
    }

    //An output flattening is as follows:
    //If there is more than one assignment to the port or it is read internally, then the port should be converted to variable
    private def flattenPortOut(block : DFDesign.Block, p : DFAny.Port.Out[_ <: DFAny.Type,_ <: DFAny.Modifier.Port.Out]) : List[(DFMember, Patch)] = {
      val refsOfPort = designDB.memberTable(p)
      val toRefs = refsOfPort.flatMap {
        case r : DFNet.ToRef => Some(r)
        case _ => None
      }

      if (toRefs.size > 1) {
        p -> Patch.ReplaceWith(DFAny.NewVar(p.dfType, DFAny.NewVar.Uninitialized, p.ownerRef, p.tags).setName(s"${block.name}_${p.name}"))
      } else {

      }
      assert(toRefs.size == 1) //currently assuming there is always one an only one connected input. TODO: fix for empty
      val aliasedRefs = refsOfPort.flatMap {
        case a : DFAny.Alias.RelValRef[_] => Some(a)
        case _ => None
      }
      assert(aliasedRefs.isEmpty)//TODO:fix for aliased connection
      val toRef = toRefs.head
      val ownerMembers = designDB.ownerMemberTable(block.getOwnerDesign) //TODO: perhaps at any hierarchy?
      val (connectedToPort, unusedNet) = ownerMembers.collectFirst{
        case m : DFNet.Connection if m.toRef == toRef => (designDB.refTable(m.fromRef), m)
      }.get
      println(s"removing unused net ${unusedNet.codeString}")
      List((p : DFMember, Patch.ReplaceWith(connectedToPort)), (unusedNet, Patch.Remove))
    }
    def flatten(design : DFDesign) : DFDesign.DB = {
      val block = design.block
      if (block.isTop) designDB else {
        val members = designDB.ownerMemberTable(block)
        val owner = block.getOwnerDesign
        val removalOrRenamePatch = (block -> Patch.ReplaceWith(owner)) :: members.flatMap {
          case p : DFAny.Port.In[_,_] => flattenPortIn(block, p)
//          case p : DFAny.Port.Out[_,_] => flattenPortOut(block, p)
          case m if !m.isAnonymous => List(m -> Patch.ReplaceWith(m.setName(s"${block.name}_${m.name}")))
          case _ => None
        }
        designDB.patch(removalOrRenamePatch)
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