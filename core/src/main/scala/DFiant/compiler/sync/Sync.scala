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

private[compiler] object Sync {
  sealed trait Tag extends DFAny.CustomTag
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
    def unapply(arg : DFAny) : Boolean = arg.tags.customTags.contains(Tag.Rst)
  }
  object IsClock {
    def unapply(arg : DFAny) : Boolean = arg.tags.customTags.contains(Tag.Clk)
  }

  object IfBlock {
    def unapply(cb : ConditionalBlock.IfBlock)(implicit getSet: MemberGetSet) : Option[DFAny] = (cb.condRef.get : DFAny) match {
      case DFAny.Func2.Unref(_, rst @ IsReset(), DFAny.Func2.Op.==, _, _, _) => Some(rst)
      case x => x.getOwner match {
        case DFInlineComponent.Block(EdgeDetect.Rep.Unref(clk @ IsClock(), _)) => Some(clk)
        case _ => None
      }
    }
  }
  object ElseIfBlock {
    def unapply(cb : ConditionalBlock.ElseIfBlock)(implicit getSet: MemberGetSet) : Option[DFAny] = cb.condRef.get.getOwner match {
      case DFInlineComponent.Block(EdgeDetect.Rep.Unref(clk @ IsClock(), _)) => Some(clk)
      case _ => None
    }
  }
  object Net {
    def unapply(net : DFNet)(implicit getSet: MemberGetSet) : Boolean = net.toRef.get.tags.customTags.contains(Sync.Tag.Reg)
  }
}

final case class ClockParams(name : String, edge : ClockParams.Edge = ClockParams.Edge.Rising) extends DFDesign.Block.CustomTag
object ClockParams {
  type Edge = EdgeDetect.Edge
  final val Edge = EdgeDetect.Edge
  final val default = ClockParams("clk", Edge.Rising)
  def get(implicit getSet: MemberGetSet) : ClockParams = getSet.designDB.top.tags.customTags.collectFirst {
    case cp : ClockParams => cp
  }.getOrElse(default)
}

final case class ResetParams(name : String, mode : ResetParams.Mode, active : ResetParams.Active) extends DFDesign.Block.CustomTag
object ResetParams {
  sealed trait Mode extends Product with Serializable
  object Mode {
    case object Async extends Mode
    case object Sync extends Mode
  }
  sealed trait Active extends Product with Serializable
  object Active {
    case object Low extends Active
    case object High extends Active
  }
  final val default = ResetParams("rst", Mode.Async, Active.Low)
  def get(implicit getSet: MemberGetSet) : ResetParams = getSet.designDB.top.tags.customTags.collectFirst {
    case cp : ResetParams => cp
  }.getOrElse(default)
}
