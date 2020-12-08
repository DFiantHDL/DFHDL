package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.mutable
import DFAny.Alias.Prev.Kind
final class SingleStepPrev[D <: DFDesign](c: IRCompilation[D]) {
  //TODO: currently assuming explicitPrev has run
  private val designDB =
    c.fixAnonymous.namedPrev
      .uniqueNames(Set(), caseSensitive = true)
      .db
  import designDB.__getset
  //in case of a.prev.prev.prev, we treat it like a.prev(3)
  @tailrec private def getRelValStep(
      relValRef: DFAny.Alias.RelValRef,
      step: Int,
      kind: Kind
  ): (DFAny.Member, Int) = {
    val relVal = relValRef.get
    relVal match {
      case DFAny.Alias.Prev(_, nextRelValRef, nextStep, nextKind, _, _)
          if nextKind == kind =>
        getRelValStep(nextRelValRef, step + nextStep, kind)
      case _ => (relVal, step)
    }
  }
  def singleStepPrev: IRCompilation[D] = {
    val namedPrevTable: mutable.Map[DFAny.Member, List[DFAny.Member]] =
      mutable.Map()
    val sigNamePattern = "(.*)_sig".r
    val patchList = designDB.members.flatMap {
      case p @ DFAny.Alias.Prev(_, _relValRef, _step, _kind, _, _) =>
        val (relVal, step)  = getRelValStep(_relValRef, _step, _kind)
        val prevNamePattern = s"(.*_${_kind.op})([0-9]+)".r
        if ((step > 1) || p.isAnonymous) { //steps require naming
          val prevList = namedPrevTable.getOrElse(relVal, List())
          if (step > prevList.length) {
            val dsn = new MetaDesign() {
              val (_, updatedPrevList) =
                (prevList.length + 1 to step)
                  .foldLeft((prevList.headOption.getOrElse(relVal), prevList)) {
                    case ((rv, list), s) =>
                      val prevName =
                        if ((step == s) & !p.isAnonymous) p.name
                        else
                          rv.name match {
                            case prevNamePattern(pre, num) =>
                              s"$pre${num.toInt + 1}"
                            case sigNamePattern(pre) =>
                              s"${pre}_${_kind.op}${s - prevList.length}"
                            case _ =>
                              s"${rv.name}_${_kind.op}${s - prevList.length}"
                          }
                      val newPrev =
                        rv.asValOf[DFAny.Type].prev.setName(prevName)
                      (newPrev, newPrev :: list)
                  }
            }
            namedPrevTable.update(relVal, dsn.updatedPrevList)
            List(
              relVal -> Patch.Add(dsn, Patch.Add.Config.After),
              p -> Patch.Replace(
                dsn.updatedPrevList.head,
                Patch.Replace.Config.ChangeRefAndRemove
              )
            )
          } else
            Some(
              p -> Patch.Replace(
                prevList(prevList.length - step),
                Patch.Replace.Config.ChangeRefAndRemove
              )
            )
        } else { //single name prev step
          val prevList = namedPrevTable.getOrElseUpdate(relVal, List(p))
          Some(
            p -> Patch.Replace(
              prevList(step - 1),
              Patch.Replace.Config.ChangeRefAndRemove
            )
          )
        }
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }
}
