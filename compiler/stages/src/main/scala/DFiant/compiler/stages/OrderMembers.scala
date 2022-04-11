package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

import scala.annotation.tailrec

private class OrderMembers(order: OrderMembers.Order)(db: DB) extends Stage(db):
  @tailrec private def orderMembers(
      remaining: List[DFMember],
      retList: List[DFMember]
  ): List[DFMember] = remaining match
    case (block: DFOwnerNamed) :: mList =>
      val members = designDB.namedOwnerMemberTable(block)
      val sortedMembers = block match
        case _: DFBlock => members.sortBy(order())
        case _          => members
      orderMembers(sortedMembers ++ mList, block :: retList)
    case m :: mList => orderMembers(mList, m :: retList)
    case Nil        => retList.reverse

  override def transform: DB =
    designDB.copy(members = orderMembers(List(designDB.top), List()))

end OrderMembers

object OrderMembers:
  trait Order:
    def apply()(using MemberGetSet): DFMember => Int
  object Order:
    given Simple: Order with
      def apply()(using MemberGetSet): DFMember => Int = {
        case dcl: DFVal.Dcl if dcl.isPort => 1
        case _: DFVal.Dcl                 => 2
        case _: DFDesignBlock             => 3
//        case DFNet.Connection(_, dcl: DFVal.Dcl, _) if dcl.modifier == DFVal.Modifier.IN => 4
//        case DFNet.Connection(toVal : DFVal.Dcl, fromVal : DFVal.Dcl, _) if toVal.isVar && fromVal.isVar =>
//          4
//        case DFNet.Connection(dcl: DFVal.Dcl, _, _) => 6
        case _ => 5
      }
//    val GuardedLast: Order = new Order:
//      def apply()(using MemberGetSet): DFMember => Int = {
//        case DFAny.Port.In() | DFAny.Port.Out() => 1
//        case DFAny.NewVar()                     => 2
//        case _: DFDesign.Block.Internal         => 3
//        case n: DFNet if n.isConnection         => 5
//        case _: CanBeGuarded                    => 5
//        case _                                  => 4
//      }
//    val LazyConnectionLast: Order = new Order:
//      def apply()(using MemberGetSet): DFMember => Int = {
//        case DFAny.Port.In() | DFAny.Port.Out() => 1
//        case DFAny.NewVar()                     => 2
//        case _: DFDesign.Block.Internal         => 3
//        case DFNet.LazyConnection(_, _, _, _)   => 6
//        case _                                  => 5
//      }
  end Order
end OrderMembers

extension [T: HasDB](t: T)
  def simpleOrder: DB = new OrderMembers(OrderMembers.Order.Simple)(t.db).transform
