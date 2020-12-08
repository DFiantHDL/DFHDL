package DFiant
package compiler

import DFiant.DFDesign.DB.Patch
import analysis.MatchHeaderAnalysis

final class ConvertMatchToIf[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def convertMatchToIf(
      matchHeaders: MemberGetSet => Iterable[DFConditional.MatchHeader]
  ): IRCompilation[D] = {
    val patchList = matchHeaders(designDB.__getset).flatMap { mh =>
      val cases    = mh.getCases
      val matchVal = mh.matchValRef.get
      val condDsn = new MetaDesign() {
        //we declare all the match conditions
        val conds = cases.map {
          case mc @ DFConditional.CaseBlock(_, _, Some(pattern), _, _) =>
            Some(
              pattern
                .matchCond(matchVal.asValOf[pattern.TType])
                .anonymize
                .member
            )
          case _ => None
        }
      }
      val casePatches =
        (cases
          .lazyZip(condDsn.conds))
          .foldLeft[
            (List[(DFMember, Patch)], Option[DFConditional.IfElseBlock])
          ](List(), None) {
            case ((patches, prevIfOption), (mc, condOption)) =>
              val dsn = new MetaDesign() {
                val ifElseBlock =
                  DFConditional.IfElseBlock(condOption, prevIfOption)
              }
              (
                mc -> Patch.Add(
                  dsn,
                  Patch.Add.Config.ReplaceWithFirst()
                ) :: patches,
                Some(dsn.ifElseBlock)
              )
            case t =>
              println(t)
              ???
          }
          ._1
      casePatches ++ List(
        mh -> Patch.Add(condDsn, Patch.Add.Config.Before),
        mh -> Patch.Remove
      )
    }
    c.newStage(designDB.patch(patchList))
  }
}
