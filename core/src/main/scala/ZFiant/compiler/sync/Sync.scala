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
package compiler.sync

object Sync {
  sealed trait Tag extends DFMember.CustomTag
  object Tag {
    case object Clk extends Tag {
      override def toString: String = "Sync.Tag.Clk"
    }
    case object Rst extends Tag {
      override def toString: String = "Sync.Tag.Rst"
    }
    case object Reg extends Tag {
      override def toString: String = "Sync.Tag.Reg"
    }
  }

  object IfBlock {
    def unapply(cb : ConditionalBlock.IfBlock)(implicit getSet: MemberGetSet) : Boolean = (cb.condRef.get : DFAny) match {
      case DFAny.Func2(_, leftArgRef, _, _, _, _) => leftArgRef.get.tags.customTags.contains(Tag.Rst)
      case x => x.getOwner match {
        case DFInlineComponent.Block(Rising.Rep(bitRef)) => bitRef.get.tags.customTags.contains(Tag.Clk)
        case _ => false
      }
    }
  }
}
