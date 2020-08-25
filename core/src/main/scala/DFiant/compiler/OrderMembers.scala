package DFiant
package compiler

import scala.annotation.tailrec
import analysis.DFAnyAnalysis

final class OrderMembers[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def orderMembers(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
    remaining match {
      case (block : DFBlock) :: mList =>
        val members = designDB.ownerMemberTable(block)
        val sortedMembers = block match {
          case _ : DFDesign.Block =>
            members.groupBy {
              case DFAny.Port.In() | DFAny.Port.Out() => 1
              case DFAny.NewVar() => 2
              case _ : DFDesign.Block.Internal => 3
//              case net : DFNet.Connection if net.hasLateConstruction => 5
//              case DFNet.Connection.Unref(DFAny.In(), DFAny.Out(), _, _) => 4
//              case DFNet.Connection.Unref(_, DFAny.Out(), _, _) => 4
//              case DFNet.Connection.Unref(DFAny.In(), _, _, _) => 6
              case _ : DFNet.Connection => 5
              case _ : CanBeGuarded => 5 //TODO: perhaps connection is not required if connections are treated better in backend compilation
              case _ => 4
            }.toList.sortBy(_._1).flatMap(_._2)
          case _ => members
        }
        orderMembers(sortedMembers ++ mList, block :: retList)
      case m :: mList => orderMembers(mList, m :: retList)
      case Nil => retList.reverse
    }

  def orderMembers : IRCompilation[D] =
    c.newStage(designDB.copy(members = orderMembers(List(designDB.top), List())))
}