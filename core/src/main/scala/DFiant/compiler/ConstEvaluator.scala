package DFiant
package compiler

import DFiant.DFAny.Token
import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter._

object ConstEvaluator extends ElementEvaluator[DFAny.Token] {
  def applyElement(token: DFAny.Token, element: SourceElement): DFAny.Token = {
    val tokenBits   = token.bitsWL(element.relWidth, element.relBitLow)
    val tokenInvert = if (element.inverted) ~tokenBits else tokenBits
    val tokenReverse =
      if (element.reversed) tokenInvert.reverse else tokenInvert
    element.dfType.getTokenFromBits(tokenReverse)
  }

  private def concat(
      dfType: DFAny.Type,
      elements: List[DFAny.Token]
  ): DFAny.Token =
    elements match {
      case head :: Nil => head
      case Nil         => dfType.getBubbleToken
      case _ =>
        dfType.getTokenFromBits(elements.map(_.bits).reduce(_ ++ _))
    }

  def apply(element: SourceElement)(implicit
      evaluationMap: EvaluationMap[DFAny.Token],
      dependencyContext: DependencyContext
  ): DFAny.Token = {
    import dependencyContext.getSet
    element.srcVal match {
      case SourceValue.Const(token) => token.applyElement(element)
      case SourceValue.Dcl(dcl, _) if dcl.isTopLevelInput =>
        element.dfType.getBubbleToken
      case SourceValue.Dcl(dcl, version) =>
        version match {
          //A declaration without an assignment at all is a constant according to its initialization
          case SourceVersion.Empty
              if !dependencyContext.assignmentMap.contains(dcl) =>
            implicit val initEvaluator = InitEvaluator
            dependencyContext.initMap.evaluate(element) match {
              case token +: _ => token
              case _          => element.dfType.getBubbleToken
            }
          //A declaration with an assignment but that is currently consumed as empty will forever
          //yield a `prev` token which in this case is never a constant.
          case SourceVersion.Empty => element.dfType.getBubbleToken
          //All other declaration accesses rely on the dependency of the relevant assignment
          case SourceVersion.Idx(block, idx) =>
            concat(
              element.dfType,
              dependencyContext
                .assignmentMap(dcl)(block)(idx)
                ._2
                .elements
                .map(evaluationMap.evaluate)
            ).applyElement(element)
          case SourceVersion.Latest =>
            concat(
              element.dfType,
              dcl.getSource.elements.map(evaluationMap.evaluate)
            ).applyElement(element)
          case SourceVersion.IfElse(_, branchVersions, fallbackVersion) =>
            val fallBackToken =
              evaluationMap.evaluate(element.versioned(dcl, fallbackVersion))
            val ifElseToken = branchVersions.foldRight(fallBackToken) {
              case ((condSrc, thenVersion), elseToken) =>
                val condToken = evaluationMap.evaluate(condSrc.elements.head)
                val thenToken =
                  evaluationMap.evaluate(element.versioned(dcl, thenVersion))
                condToken match {
                  case b: DFBool.Token => b.select(thenToken, elseToken)
                  case _               => ??? //condition should always be boolean
                }
            }
            ifElseToken
          case SourceVersion.Match(
                headCase,
                caseVersions,
                fallbackVersionOption
              ) =>
            element.dfType.getBubbleToken
        }
      case SourceValue.Func2(func) =>
        val leftSource  = func.leftArgRef.getSource
        val rightSource = func.rightArgRef.getSource
        val leftToken = concat(
          leftSource.dfType,
          leftSource.elements.map(evaluationMap.evaluate)
        )
        val rightToken = concat(
          rightSource.dfType,
          rightSource.elements.map(evaluationMap.evaluate)
        )
        func.tokenFunc(leftToken, rightToken).applyElement(element)
      case SourceValue.ApplySel(_, dfVal, idxSrc) => ???
    }
  }

  def cyclic(element: SourceElement)(implicit
      evaluationMap: EvaluationMap[DFAny.Token],
      dependencyContext: DependencyContext
  ): DFAny.Token = element.dfType.getBubbleToken

  def codeString(t: DFAny.Token)(implicit printer: CSPrinter): String =
    t.codeString.unformatted
}
