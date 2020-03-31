package DFiant
package compiler

import scala.annotation.tailrec
import scala.collection.immutable

final case class AssignedScope(latest : immutable.BitSet, branchHistory : Option[immutable.BitSet], parentScopeOption : Option[AssignedScope]) {
  @tailrec private def getLatest(latest : immutable.BitSet, parentScopeOption : Option[AssignedScope]) : immutable.BitSet =
    parentScopeOption match {
      case Some(s) => getLatest(latest | s.latest, s.parentScopeOption)
      case None => latest
    }
  def getLatest : immutable.BitSet = getLatest(latest, parentScopeOption)
  def isConsumingPrevAt(consumeBitSet : immutable.BitSet) : Boolean = (consumeBitSet &~ getLatest).nonEmpty
  def assign(assignBitSet : immutable.BitSet) : AssignedScope = copy(latest | assignBitSet)
  def branchEntry(firstBranch : Boolean) : AssignedScope = {
    val parentScope = if (firstBranch) this.copy(branchHistory = Some(getLatest)) else this
    AssignedScope(immutable.BitSet(), None, Some(this))
  }
  def branchExit(lastBranch : Boolean, exhaustive : Boolean) : AssignedScope = parentScopeOption match {
    case Some(parentScope) =>
      val updatedHistory = parentScope.branchHistory match {
        case Some(h) => latest & h
        case None => latest
      }
      if (lastBranch) {
        if (exhaustive) AssignedScope(parentScope.latest | updatedHistory, None, parentScope.parentScopeOption)
        else AssignedScope(parentScope.latest, None, parentScope.parentScopeOption)
      } else
        AssignedScope(parentScope.latest, Some(updatedHistory), parentScope.parentScopeOption)
    case None => this
  }
}
object AssignedScope {
  val empty : AssignedScope = AssignedScope(immutable.BitSet(), None, None)
}
