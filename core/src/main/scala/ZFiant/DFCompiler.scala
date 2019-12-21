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
        case (block, members) => members.groupBy(m => m.meta.namePosition).flatMap {
          //In case an anonymous member got a name from its owner. For example:
          //val ret = DFBits(8).ifdf(cond) {
          //  i & i
          //}
          //The `i & i` function would also get the `ret` name just as the if block itself
          case (pos, gm) if (pos == block.meta.namePosition) => gm
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
      designDB.patch(anonymizeList.map(a => a -> a.anonymize).toMap)
    }
  }

  final implicit class CodeString(designDB : DFDesign.DB) {
    import designDB.getset
    def blockBodyCodeString(block : DFBlock, members : List[DFMember]) : String = {
      val membersCodeString = members.collect {
        case mh : ConditionalBlock.MatchHeader[_] => mh.codeString
        case cb : ConditionalBlock[_] => cb.codeString(blockBodyCodeString(cb, designDB.ownerMemberTable(cb)))
        case m : DFDesign.Block => s"val ${m.name} = new ${m.typeName} {}" //TODO: fix
        case n : DFNet => n.codeString
        case a : DFAny if !a.meta.name.anonymous => s"val ${a.name} = ${a.codeString}"
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