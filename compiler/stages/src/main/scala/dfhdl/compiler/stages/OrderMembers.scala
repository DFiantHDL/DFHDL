package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

import scala.annotation.tailrec

private abstract class OrderMembers(order: OrderMembers.Order) extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  @tailrec private def orderMembers(
      remaining: List[DFMember],
      retList: List[DFMember]
  )(using MemberGetSet): List[DFMember] = remaining match
    case (block: DFOwnerNamed) :: mList =>
      val members = getSet.designDB.namedOwnerMemberTable(block)
      val sortedMembers = block match
        case _: DFDesignBlock => members.sortBy(order())
        case _                => members
      orderMembers(sortedMembers ++ mList, block :: retList)
    case m :: mList => orderMembers(mList, m :: retList)
    case Nil        => retList.reverse

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    designDB.copy(members = orderMembers(List(designDB.top), List()))

end OrderMembers

object OrderMembers:
  trait Order:
    def apply()(using MemberGetSet): DFMember => Int
  object Order:
    object Simple extends Order:
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

case object SimpleOrderMembers extends OrderMembers(OrderMembers.Order.Simple)
extension [T: HasDB](t: T)
  def simpleOrder: DB =
    StageRunner.run(SimpleOrderMembers)(t.db)(using dfhdl.options.CompilerOptions.default)
