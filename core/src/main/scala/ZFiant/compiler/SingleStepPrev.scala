package ZFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.mutable

final class SingleStepPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.explicitPrev.fixAnonymous.db
  import designDB.__getset
  //in case of a.prev.prev.prev, we treat it like a.prev(3)
  @tailrec private def getRelValStep(relValRef : DFAny.Alias.RelValRef[DFAny], step : Int) : (DFAny, Int) = {
    val relVal = relValRef.get
    relVal match {
      case DFAny.Alias.Prev(_, nextRelValRef, nextStep, _, _) => getRelValStep(nextRelValRef, step + nextStep)
      case _ => (relVal, step)
    }
  }
  def singleStepPrev = {
    val namedPrevTable : mutable.Map[DFAny, List[DFAny]] = mutable.Map()
    val prevNamePattern = "(.*_prev)([0-9]+)".r
    val patchList = designDB.members.flatMap {
      case p @ DFAny.Alias.Prev(_, _relValRef, _step, _, _) =>
        val (relVal, step) = getRelValStep(_relValRef, _step)
        if ((step > 1) || p.isAnonymous) { //steps require naming
          val prevList = namedPrevTable.getOrElse(relVal, List())
          if (step > prevList.length) {
            val dsn = new MetaDesign() {
              val (_, updatedPrevList) = (prevList.length + 1 to step).foldLeft((prevList.headOption.getOrElse(relVal), prevList)) { case ((rv, list), s) =>
                val prevName = if ((step == s) & !p.isAnonymous) p.name else rv.name match {
                  case prevNamePattern(pre, num) => s"$pre${num.toInt + 1}"
                  case _ => s"${rv.name}_prev${s - prevList.length}"
                }
                val newPrev = rv.asInstanceOf[DFAny.Of[DFAny.Type]].prev().setName(prevName)
                (newPrev, newPrev :: list)
              }
            }
            namedPrevTable.update(relVal, dsn.updatedPrevList)
            Some(p -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast))
          } else Some(p -> Patch.Replace(prevList(prevList.length - step), Patch.Replace.Config.ChangeRefOnly))
        } else { //single name prev step
          namedPrevTable.update(relVal, List(p))
          None
        }
      case _ => None
    }
    c.newStage[SingleStepPrev](designDB.patch(patchList), Seq())
  }
}

trait SingleStepPrev extends Compilable.Stage
