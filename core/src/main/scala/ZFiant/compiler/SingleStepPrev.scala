package ZFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.mutable

final class SingleStepPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.explicitPrev.fixAnonymous.db
  import designDB.__getset

  def singleStepPrev = {
    val namedPrevTable : mutable.Map[DFAny, List[DFAny]] = mutable.Map()
    designDB.members.flatMap {
      case p @ DFAny.Alias.Prev(_, relValRef, step, _, _) =>
        val relVal = relValRef.get.asInstanceOf[DFAny.Of[DFAny.Type]]
        if ((step > 1) || p.isAnonymous) { //steps require naming
          val prevList = namedPrevTable.getOrElse(relVal, List())
          if (step > prevList.length) {
            val dsn = new MetaDesign() {
              val prevVals = (prevList.length + 1 to step).foldLeft(relVal) { case (rv, s) =>
                val prevName = s"${relVal.name}_prev$s"
                rv.prev().setName(prevName)
              }
            }

          }
          None
        } else { //single name prev step
          namedPrevTable.update(relVal, List(p))
          None
        }
      case _ => None
    }
//    val explicitPrevSet = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
//    val patchList = explicitPrevSet.toList.map(e => e -> Patch.Add(new MetaDesign() {
//      DFNet.Assignment(e, DFAny.Alias.Prev(e, 1))
//    }, Patch.Add.Config.After))
//
//          println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
//    c.newStage[SingleStepPrev](designDB.patch(patchList), Seq())
    ???
  }
}

trait SingleStepPrev extends Compilable.Stage
