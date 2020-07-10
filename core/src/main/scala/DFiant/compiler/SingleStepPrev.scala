package DFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch
import DFiant.csprinter.CSPrinter

import scala.annotation.tailrec
import scala.collection.mutable

final class SingleStepPrevOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  //TODO: currently assuming explicitPrev has run
  private val designDB = c.fixAnonymous.db
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
    val sigNamePattern = "(.*)_sig".r
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
                  case sigNamePattern(pre) => s"${pre}_prev${s - prevList.length}"
                  case _ => s"${rv.name}_prev${s - prevList.length}"
                }
                val newPrev = rv.asInstanceOf[DFAny.Of[DFAny.Type]].prev.setName(prevName)
                (newPrev, newPrev :: list)
              }
            }
            namedPrevTable.update(relVal, dsn.updatedPrevList)
            List(
              relVal -> Patch.Add(dsn, Patch.Add.Config.After),
              p -> Patch.Replace(dsn.updatedPrevList.head, Patch.Replace.Config.ChangeRefAndRemove)
            )
          } else
            Some(p -> Patch.Replace(prevList(prevList.length - step), Patch.Replace.Config.ChangeRefAndRemove))
        } else { //single name prev step
          val prevList = namedPrevTable.getOrElseUpdate(relVal, List(p))
          Some(p -> Patch.Replace(prevList(step - 1), Patch.Replace.Config.ChangeRefAndRemove))
        }
      case _ => None
    }
    c.newStage[SingleStepPrev](designDB.patch(patchList))
  }
}

trait SingleStepPrev extends Compilation.Stage
