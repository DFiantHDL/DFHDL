package DFiant
package compiler

import DFiant.DFDesign.DB.Patch
import analysis.MatchHeaderAnalysis

final class ForceOthersCaseCoverage[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def forceOthersCaseCoverage(
      matchHeaders: MemberGetSet => Iterable[DFConditional.MatchHeader]
  ): IRCompilation[D] = {
    val patchList = matchHeaders(__getset).flatMap { mh =>
      val cases = mh.getCases
      cases.last match {
        case DFConditional.CaseBlock(_, _, None, _, _) =>
          None //already has a case_ block
        case cb =>
          val assignedVars: Iterable[DFAny.VarOf[DFAny.Type]] =
            mh.getExternalAssignedVars.map(_.asVarOf[DFAny.Type])
          val dsn = new MetaDesign() {
            val case_ = DFConditional.CaseBlock(mh, Some(cb), None)
            applyBlock(case_) {
              assignedVars.foreach(a => a := ?)
            }
          }
          Some(cb -> Patch.Add(dsn, Patch.Add.Config.After))
      }
    }
    c.newStage(designDB.patch(patchList))
  }
}
