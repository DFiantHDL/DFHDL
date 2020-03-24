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

private[compiler] object Sync {
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
  object ElseIfBlock {
    def unapply(cb : ConditionalBlock.ElseIfBlock)(implicit getSet: MemberGetSet) : Boolean = cb.condRef.get.getOwner match {
      case DFInlineComponent.Block(Rising.Rep(bitRef)) => bitRef.get.tags.customTags.contains(Tag.Clk)
      case _ => false
    }
  }
  object Net {
    def unapply(net : DFNet)(implicit getSet: MemberGetSet) : Boolean = net.toRef.get.tags.customTags.contains(Sync.Tag.Reg)
  }
}

final case class ClockParams(name : String, edge : ClockParams.Edge = ClockParams.Edge.Rising) extends DFDesign.Block.CustomTag
object ClockParams {
  trait Edge extends Product with Serializable
  object Edge {
    case object Rising extends Edge
    case object Falling extends Edge
  }
  final val default = ClockParams("clk", Edge.Rising)
}

final case class ResetParams(name : String, mode : ResetParams.Mode, active : ResetParams.Active) extends DFDesign.Block.CustomTag
object ResetParams {
  trait Mode extends Product with Serializable
  object Mode {
    case object Async extends Mode
    case object Sync extends Mode
  }
  trait Active extends Product with Serializable
  object Active {
    case object Low extends Active
    case object High extends Active
  }
  final val default = ResetParams("rst", Mode.Async, Active.Low)
}
