package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter._

object InitEvaluator extends ElementEvaluator[Seq[DFAny.Token]] {
  def applyElement(t : Seq[DFAny.Token], element : SourceElement) : Seq[DFAny.Token] =
    if (!element.withInit) Seq() else {
      val tokenBitsSeq = t.bitsWL(element.relWidth, element.relBitLow)
      val tokenInvertSeq = if (element.inverted) DFAny.TokenSeq(tokenBitsSeq)(~_) else tokenBitsSeq
      val tokenReverseSeq = if (element.reversed) DFAny.TokenSeq(tokenBitsSeq)(_.reverse) else tokenInvertSeq
      val tokenPrevSeq = tokenReverseSeq.prevInit(element.prevStep)
      DFAny.TokenSeq(tokenPrevSeq)(element.dfType.getTokenFromBits)
    }

  private def concat(dfType : DFAny.Type, elements : List[Seq[DFAny.Token]]) : Seq[DFAny.Token] =
    elements match {
      case head :: Nil => head
      case Nil => Seq()
      case _ =>
        DFAny.TokenSeq(elements.map(_.bits).reduce((l, r) => DFAny.TokenSeq(l, r)(_ ++ _)))(dfType.getTokenFromBits)
    }

  def apply(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[Seq[DFAny.Token]], dependencyContext : DependencyContext
  ) : Seq[DFAny.Token] = if (!element.withInit) Seq() else element.srcVal match {
    case SourceValue.Const(token) => Seq(token).applyElement(element)
    case SourceValue.Dcl(dfVal, version) =>
      dfVal.externalInit match {
        //If a declaration has an init statement, then that takes precedence
        case Some(init) => init.applyElement(element)
        //Without an init statement, the connection sets the init
        case None =>
          val elements = version match {
            case SourceVersion.Empty => Nil
            case SourceVersion.Idx(block, idx) => dependencyContext.assignmentMap(dfVal)(block)(idx)._2.elements
            case SourceVersion.Latest => dfVal.getSource.elements
            case SourceVersion.IfElse(headBranch, branchVersions, fallbackVersion) => Nil
            case SourceVersion.Match(headCase, caseVersions, fallbackVersion) => Nil
          }
          concat(element.dfType, elements.map(evaluationMap.evaluate)).applyElement(element)
      }
    case SourceValue.Func2(func) =>
      val leftSource = func.leftArgRef.getSource
      val rightSource = func.rightArgRef.getSource
      val leftToken = concat(leftSource.dfType, leftSource.elements.map(evaluationMap.evaluate))
      val rightToken = concat(rightSource.dfType, rightSource.elements.map(evaluationMap.evaluate))
      func.initFunc(leftToken, rightToken).applyElement(element)
    case SourceValue.ApplySel(_, dfVal, idxSrc) => ???
  }

  def cyclic(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[Seq[DFAny.Token]], dependencyContext : DependencyContext
  ) : Seq[DFAny.Token] = Seq()

  override def getDependencies(element : SourceElement)(
    implicit dependencyContext: DependencyContext
  ) : List[SourceElement] = element.getDependencies(DependencyKind.Init)

  def codeString(t : Seq[DFAny.Token])(implicit printer: CSPrinter) : String = t.codeString.unformatted

}

