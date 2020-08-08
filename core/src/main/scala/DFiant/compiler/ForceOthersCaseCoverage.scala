//package DFiant
//package compiler
//
//import DFiant.DFDesign.DB.Patch
//import analysis.MatchHeaderAnalysis
//
//final class ForceOthersCaseCoverage[D <: DFDesign](c : IRCompilation[D]) {
//  private val designDB = c.db
//  import designDB.__getset
//  def forceOthersCaseCoverage(matchHeaders : MemberGetSet => Iterable[ConditionalBlock.MatchHeader]) : IRCompilation[D] = {
//    val patchList = matchHeaders(designDB.__getset).flatMap { mh =>
//      val cases = mh.getCases
//      cases.last match {
//        case _ : ConditionalBlock.Case_Block[_] => None //already has a case_ block
//        case block : ConditionalBlock.CasePatternBlock[_] =>
//          val matchVal = mh.matchValRef.get
//          val assignedVars : List[DFAny.VarOf[DFAny.Type]] = Nil
//          val dsn = new MetaDesign() {
//            block match {
//              case cb : ConditionalBlock.NoRetVal.DFCasePatternBlock[_] =>
//                cb.casedf_ {
//                  assignedVars.foreach(a => a := ?)
//                }
//              case cb : ConditionalBlock.WithRetVal.DFCasePatternBlock[_,_] =>
//                val retVar = cb.retVarRef.get//.asInstanceOf[DFAny.VarOf[DFAny.Type]]
//                val block = {
//                  assignedVars.filterNot(_ == retVar).foreach(_ := ?)
//                  DFAny.Const(retVar.dfType, retVar.dfType.getBubbleToken)
//                }
//                ConditionalBlock.WithRetVal.DFCase_Block(retVar, mh, cb)(???)
//            }
//          }
//          ???
//      }
//    }
//    c.newStage(designDB.patch(patchList))
//  }
//}