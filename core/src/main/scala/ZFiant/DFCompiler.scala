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
    def fixNames : DFDesign.DB = {
      import designDB.getter
      val patchList = designDB.members.collect {
        case m : DFAny if (m.meta.name == m.getOwner.meta.name) && (m.meta.namePosition == m.getOwner.meta.namePosition) =>
          m -> m.anonymize
        case m : DFAny.Const[_] => m -> m.anonymize
      }
      designDB.patch(patchList.toMap)
    }
  }

  final implicit class CodeString(designDB : DFDesign.DB) {
    import designDB.getter
    def blockBodyCodeString(block : DFBlock, members : List[DFMember]) : String = {
      val membersCodeString = members.collect {
        case mh : ConditionalBlock.MatchHeader[_] => mh.codeString
        case cb : ConditionalBlock[_] => cb.codeString(blockBodyCodeString(cb, designDB.ownerMemberTable(cb)))
        case m : DFBlock => s"val $m = new ${m.typeName} {}" //TODO: fix
        case n : DFNet => n.codeString
        case a : DFAny if !a.meta.name.anonymous => s"val ${a.name} = ${a.codeString}"
      }
      membersCodeString.mkString("\n")
    }
    def codeString : String = {
      designDB.ownerMemberList.map{case (block, members) => blockBodyCodeString(block, members)}.mkString("\n")
    }
    def printCodeString() : DFDesign.DB = {
      println(codeString)
      designDB
    }
  }
}