package ZFiant
package compiler

import DFDesign.DB.Patch
import scala.annotation.tailrec

final class UtilsOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
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
}

trait Utils extends Compilable.Stage