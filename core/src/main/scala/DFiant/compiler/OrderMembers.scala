package DFiant
package compiler

import scala.annotation.tailrec
import analysis.DFAnyAnalysis

final class OrderMembers[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def orderMembers(remaining : List[DFMember], retList : List[DFMember])(
    implicit order : OrderMembers.Order
  ) : List[DFMember] = remaining match {
    case (block : DFBlock) :: mList =>
      val members = designDB.ownerMemberTable(block)
      val sortedMembers = block match {
        case _ : DFDesign.Block => members.sortBy(order(__getset))
        case _ => members
      }
      orderMembers(sortedMembers ++ mList, block :: retList)
    case m :: mList => orderMembers(mList, m :: retList)
    case Nil => retList.reverse
  }

  def orderMembers(implicit order : OrderMembers.Order) : IRCompilation[D] =
    c.newStage(designDB.copy(members = orderMembers(List(designDB.top), List())))
}

object OrderMembers {
  trait Order {
    def apply(implicit getSet: MemberGetSet) : DFMember => Int
  }
  object Order {
    implicit val Simple : Order = new Order {
      def apply(implicit getSet : MemberGetSet) : DFMember => Int = {
        case DFAny.Port.In() | DFAny.Port.Out() => 1
        case DFAny.NewVar() => 2
        case _ : DFDesign.Block.Internal => 3
        case DFNet.Connection(_, DFAny.In(), _, _) => 4
        case DFNet.Connection(DFAny.NewVar(), v @ DFAny.NewVar(), _, _) if !v.isNonAliasAssigned => 4
        case DFNet.Connection(DFAny.Out(), _, _, _) => 6
        case DFNet.Assignment(v @ DFAny.Out(), _, _, _) if getSet.designDB.getAssignmentsTo(v).size == 1 => 6
        case _ => 5
      }
    }
    val GuardedLast : Order = new Order {
      def apply(implicit getSet : MemberGetSet) : DFMember => Int = {
        case DFAny.Port.In() | DFAny.Port.Out() => 1
        case DFAny.NewVar() => 2
        case _ : DFDesign.Block.Internal => 3
        case n : DFNet if n.isConnection => 5
        case _ : CanBeGuarded => 5
        case _ => 4
      }
    }
    val LazyConnectionLast : Order = new Order {
      def apply(implicit getSet : MemberGetSet) : DFMember => Int = {
        case DFAny.Port.In() | DFAny.Port.Out() => 1
        case DFAny.NewVar() => 2
        case _ : DFDesign.Block.Internal => 3
        case DFNet.LazyConnection(_,_,_,_) => 6
        case _ => 5
      }
    }
  }
}