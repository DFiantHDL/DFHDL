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

package DFiant
package compiler.sync

import DFiant.compiler.sync

private[compiler] object Sync {
  sealed trait Tag extends DFMember.CustomTagOf[DFAny]
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

  object IsReset {
    def unapply(arg : DFAny)(implicit getSet: MemberGetSet) : Boolean = arg.isTaggedWith(Tag.Rst)
  }
  object IsClock {
    def unapply(arg : DFAny)(implicit getSet: MemberGetSet) : Boolean = arg.isTaggedWith(Tag.Clk)
  }

  object IfBlock {
    def unapply(cb : ConditionalBlock.IfBlock)(implicit getSet: MemberGetSet) : Option[(DFAny, Boolean)] = (cb.condRef.get : DFAny) match {
      case DFAny.Func2.Unref(_, rst @ IsReset(), DFAny.Func2.Op.==, DFAny.Const(_, DFBool.Token(_,edge,_),_,_), _, _) => Some(rst, edge)
      case x => x.getOwnerBlock match {
        case DFInlineComponent.Block(EdgeDetect.Rep.Unref(clk @ IsClock(), edge)) => Some(clk, edge == EdgeDetect.Edge.Rising)
        case _ => None
      }
    }
  }
  object ElseIfBlock {
    def unapply(cb : ConditionalBlock.ElseIfBlock)(implicit getSet: MemberGetSet) : Option[(DFAny, Boolean)] = cb.condRef.get.getOwnerBlock match {
      case DFInlineComponent.Block(EdgeDetect.Rep.Unref(clk @ IsClock(), edge)) => Some(clk, edge == EdgeDetect.Edge.Rising)
      case _ => None
    }
  }
  object Net {
    def unapply(net : DFNet)(implicit getSet: MemberGetSet) : Boolean = net.toRef.get.isTaggedWith(Sync.Tag.Reg)
  }
}