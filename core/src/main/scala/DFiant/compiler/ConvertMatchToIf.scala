package DFiant
package compiler

import DFiant.DFDesign.DB.Patch

final class ConvertMatchToIf[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def convertMatchToIf(matchHeaders : MemberGetSet => Iterable[ConditionalBlock.MatchHeader]) : IRCompilation[D] = {
    val patchList = matchHeaders(designDB.__getset).flatMap { mh =>
      val cases = mh.getCases
      val matchVal = mh.matchValRef.get
      val condDsn = new MetaDesign() {
        //we declare all the match conditions
        val conds = cases.map {
          case mc : ConditionalBlock.CasePatternBlock[_] =>
            Some(mc.pattern.matchCond(matchVal.asInstanceOf[DFAny.Of[mc.pattern.TType]]).anonymize)
          case mc : ConditionalBlock.Case_Block[_] => None
        }
      }
      val casePatches =
        (cases.lazyZip(condDsn.conds)).foldLeft[(List[(DFMember, Patch)], Option[ConditionalBlock])](List(), None) {
          case ((patches, None), (mc, Some(cond))) =>
            val dsn = new MetaDesign() {
              val ifBlock = ConditionalBlock.NoRetVal.IfBlock.forcedHeader(cond)
            }
            (mc -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()) :: patches, Some(dsn.ifBlock))
          case ((patches, Some(prevIf)), (mc, Some(cond))) =>
            val dsn = new MetaDesign() {
              val elseIfBlock = ConditionalBlock.NoRetVal.ElseIfBlock.forcedHeader(cond, prevIf)
            }
            (mc -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()) :: patches, Some(dsn.elseIfBlock))
          case ((patches, Some(prevIf)), (mc, None)) =>
            val dsn = new MetaDesign() {
              val elseBlock = ConditionalBlock.NoRetVal.ElseBlock.forcedHeader(prevIf)
            }
            (mc -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()) :: patches, Some(dsn.elseBlock))
          case t =>
            println(t)
            ???
        }._1
      casePatches ++ List(mh -> Patch.Add(condDsn, Patch.Add.Config.Before), mh -> Patch.Remove)
    }
    c.newStage(designDB.patch(patchList))
  }
}