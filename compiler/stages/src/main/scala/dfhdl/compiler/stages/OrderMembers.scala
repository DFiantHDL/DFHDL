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
    designDB.copy(members = designDB.membersGlobals ++ orderMembers(List(designDB.top), List()))

end OrderMembers

object OrderMembers:
  trait Order:
    def apply()(using MemberGetSet): DFMember => Int
  object Order:
    object Simple extends Order:
      def apply()(using MemberGetSet): DFMember => Int = {
        // design parameter's default value comes first
        case DefaultOfDesignParam(_) => 1
        // design parameters come second as they are dependent only on external
        // initialization or default values and everything else can depend on them
        case _: DFVal.DesignParam => 2
        // anonymous members that are referenced by declarations come third
        case dfVal: DFVal if dfVal.isReferencedByAnyDcl => 3
        // fourth to come are constant declarations that may be referenced by ports
        case DclConst() => 4
        // fifth are ports
        case DclPort() => 5
        // sixth are variables
        case DclVar() => 6
        // seventh are design blocks that are direct children of named instances
        // (e.g., design blocks inside conditional blocks are not included)
        case dsn: DFDesignBlock if dsn.getOwner == dsn.getOwnerNamed => 7
        // then the rest
        case _ => 8
      }
    end Simple
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
  def simpleOrder(using CompilerOptions): DB =
    StageRunner.run(SimpleOrderMembers)(t.db)
