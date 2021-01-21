package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter._

object InitEvaluator extends ElementEvaluator[Seq[DFAny.Token]] {
  def applyElement(t : => Seq[DFAny.Token], element : SourceElement) : Seq[DFAny.Token] =
    if (!element.withInit) Seq()
    else if (element.relWidth != element.srcVal.member.width) t.bitsWL(element.relWidth, element.relBitLow)
    else t

  protected def concat(dfType : DFAny.Type, elements : List[Seq[DFAny.Token]]) : Seq[DFAny.Token] =
    elements match {
      case head :: Nil => head
      case Nil         => Seq()
      case _ =>
        DFAny.TokenSeq(
          elements.map(_.bits).reduce((l, r) => DFAny.TokenSeq(l, r)(_ ++ _))
        )(dfType.getTokenFromBits)
    }

  def apply(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[Seq[DFAny.Token]], dependencyContext : DependencyContext
  ) : Seq[DFAny.Token] = {
    import dependencyContext.getSet
    srcVal match {
      case SourceValue(const : DFAny.Const, _) => Seq(const.token)
      case SourceValue(dcl : DFAny.Dcl, version) =>
        dcl.externalInit match {
          //If a declaration has an init statement, then that takes precedence
          case Some(init) => init
          //Without an init statement, the connection sets the init
          case None => version match {
            case SourceVersion.Empty => Seq()
            case SourceVersion.Idx(block, idx) => dependencyContext.assignmentMap(dcl)(block)(idx)._2.evaluate(dcl.dfType)
            case SourceVersion.Latest => dcl.getSource.evaluate(dcl.dfType)
            case SourceVersion.IfElse(headBranch, branchVersions, fallbackVersion) => Seq() //TODO: allow init propagation through conditionals
            case SourceVersion.Match(headCase, caseVersions, fallbackVersion) => Seq() //TODO: allow init propagation through conditionals
          }
        }
      case SourceValue(member, SourceVersion.Latest) => member match {
        case func : DFAny.Func1 =>
          func.initFunc(func.leftArgRef.evaluate)
        case func : DFAny.Func2 =>
          func.initFunc(func.leftArgRef.evaluate, func.rightArgRef.evaluate)
        case alias : DFAny.Alias =>
          alias.initFunc(alias.relValRef.evaluate)
        case applySel : DFAny.ApplySel =>
          val relValInit = applySel.relValRef.evaluate
          val idxInit = applySel.idxRef.evaluate
          DFAny.TokenSeq(relValInit, idxInit) {
            case (r : DFBits.Token, i : DFUInt.Token) => r.bit(i)
            case (r : DFVector.Token, i : DFUInt.Token) => r.sel(i)
          }
      }
      case x =>
        println(x)
        ???
    }
  }

  def cyclic(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[Seq[DFAny.Token]], dependencyContext : DependencyContext
  ) : Seq[DFAny.Token] = Seq()

  override def getDependencies(element: SourceElement)(implicit
      dependencyContext: DependencyContext
  ): List[SourceElement] = element.getDependencies(DependencyKind.Init)

  def codeString(t: Seq[DFAny.Token])(implicit printer: CSPrinter): String =
    t.codeString.unformatted

}
