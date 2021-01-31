package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter._

object ConstEvaluator extends ElementEvaluator[DFAny.Token] {
  def applyElement(token : => DFAny.Token, element : SourceElement) : DFAny.Token =
    if (element.relWidth != element.srcVal.member.width) token.bitsWL(element.relWidth, element.relBitLow)
    else token

  protected def concat(dfType : DFAny.Type, elements : List[DFAny.Token]) : DFAny.Token =
    elements match {
      case head :: Nil => head
      case Nil => dfType.getBubbleToken
      case _ =>
        dfType.getTokenFromBits(elements.map(_.bits).reduce(_ ++ _))
    }

  def apply(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[DFAny.Token], dependencyContext : DependencyContext
  ) : DFAny.Token = {
    import dependencyContext.getSet
    srcVal match {
      case SourceValue(const : DFAny.Const, _) => const.token
      case SourceValue(dcl : DFAny.Dcl, _) if dcl.isTopLevelInput =>
        dcl.dfType.getBubbleToken
      case SourceValue(dcl : DFAny.Dcl, version) =>
        version match {
          //A declaration without an assignment at all is a constant according to its initialization
          case SourceVersion.Empty if !dependencyContext.assignmentMap.contains(dcl) =>
            implicit val initEvaluator = InitEvaluator
            dependencyContext.initMap.evaluate(srcVal) match {
              case token +: _ => token
              case _ => dcl.dfType.getBubbleToken
            }
          //A declaration with an assignment but that is currently consumed as empty will forever
          //yield a `prev` token which in this case is never a constant.
          case SourceVersion.Empty => dcl.dfType.getBubbleToken
          //All other declaration accesses rely on the dependency of the relevant assignment
          case SourceVersion.Idx(block, idx) =>
            dependencyContext.assignmentMap(dcl)(block)(idx)._2.evaluate(dcl.dfType)
          case SourceVersion.Latest =>
            dcl.getSource.evaluate(dcl.dfType)
          case SourceVersion.IfElse(_, branchVersions, fallbackVersion) =>
            val fallBackToken = dcl.evaluate(fallbackVersion)
            val ifElseToken = branchVersions.foldRight(fallBackToken){ case ((condSrc, thenVersion), elseToken) =>
              val condToken = condSrc.evaluate(DFBool.Type(true))
              val thenToken = dcl.evaluate(thenVersion)
              condToken match {
                case b : DFBool.Token => b.select(thenToken, elseToken)
                case _ => ??? //condition should always be boolean
              }
            }
            ifElseToken
          case SourceVersion.Match(headCase, caseVersions, fallbackVersionOption) =>
            dcl.dfType.getBubbleToken
        }
      case SourceValue(member, SourceVersion.Latest) => member match {
        case func : DFAny.Func1 =>
          func.tokenFunc(func.leftArgRef.evaluate)
        case func : DFAny.Func2 =>
          func.tokenFunc(func.leftArgRef.evaluate, func.rightArgRef.evaluate)
        case prev : DFAny.Alias.Prev =>
          val prevConst = prev.relValRef.evaluate
          implicit val initEvaluator = InitEvaluator
          val prevInit = dependencyContext.initMap.evaluate(srcVal) match {
            case token +: _ => token
            case _ => prev.dfType.getBubbleToken
          }
          if (prevInit == prevConst) prevConst
          else prev.dfType.getBubbleToken
        case alias : DFAny.Alias =>
          alias.constFunc(alias.relValRef.evaluate)
        case applySel : DFAny.ApplySel =>
          val relValConst = applySel.relValRef.evaluate
          val idxConst = applySel.idxRef.evaluate
          (relValConst, idxConst) match {
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
    implicit srcValEvaluation : SourceValueEvaluation[DFAny.Token], dependencyContext : DependencyContext
  ) : DFAny.Token = srcVal.dfType.getBubbleToken

  def codeString(t : DFAny.Token)(implicit printer: CSPrinter) : String = t.codeString.unformatted
}

