package DFiant
package compiler

import scala.annotation.tailrec

final class OrderMembers[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  @tailrec private def orderMembers(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
    remaining match {
      case (block : DFBlock) :: mList =>
        val members = designDB.ownerMemberTable(block)
        val sortedMembers = block match {
          case _ : DFDesign.Block =>
            members.groupBy {
              case DFAny.Port.In() | DFAny.Port.Out() => 1
              case DFAny.Var() => 2
              case _ : DFDesign.Block.Internal => 3
              case _ : CanBeGuarded | _ : DFNet.Connection => 5 //TODO: perhaps connection is not required if connections are treated better in backend compilation
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