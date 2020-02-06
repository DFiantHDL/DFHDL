package ZFiant

import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.immutable

package object compiler {
  implicit class Utils[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    def fixAnonymous : DFDesign.DB = {
      val anonymizeList = designDB.designMemberList.flatMap {
        case (block, members) =>
          members.filterNot(m => m.isAnonymous).groupBy(m => (m.tags.meta.namePosition, m.name)).flatMap {
            //In case an anonymous member got a name from its owner. For example:
            //val ret = DFBits(8).ifdf(cond) {
            //  i & i
            //}
            //The `i & i` function would also get the `ret` name just as the if block itself
            case ((pos, _), gm) if (pos == block.tags.meta.namePosition) => gm
            case (_, gm) if (gm.length > 1) =>
              //In case an anonymous member was used as an argument to an owner. For example:
              //val ret = DFBits(8).ifdf(i & i) {
              //}
              //The `i & i` function would also get the `ret` name just as the if block itself
              if (gm.collectFirst{case x : DFBlock => x}.isDefined) gm.collect {case a : DFAny.CanBeAnonymous => a}
              //In case an anonymous member inside a composition, we anonymize all but the last. For example:
              //val ret = i & i | i
              //Only the final 'Or' operation would be considered for the name `ret`
              else gm.dropRight(1)
            case _ => List()
          }
      }
      designDB.patch(anonymizeList.map(a => a -> Patch.Replace(a.anonymize, Patch.Replace.Config.FullReplacement)))
    }
    def uniqueNames : DFDesign.DB = ???
    @tailrec private def mcf(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
      remaining match {
        case (block : DFBlock) :: mList =>
          val members = designDB.ownerMemberTable(block)
          val sortedMembers = block match {
            case _ : DFDesign.Block =>
              val split = members.partition {
                case _ : CanBeGuarded => false
                case _ => true
              }
              split._1 ++ split._2
            case _ => members
          }
          mcf(sortedMembers ++ mList, block :: retList)
        case m :: mList => mcf(mList, m :: retList)
        case Nil => retList.reverse
      }
    def moveConnectableFirst : DFDesign.DB = designDB.copy(members = mcf(List(designDB.top), List()))
  }

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

}
