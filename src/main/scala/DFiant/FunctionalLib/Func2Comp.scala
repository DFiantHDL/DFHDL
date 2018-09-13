package DFiant.FunctionalLib

import DFiant._
import internals._
import singleton.twoface._

abstract class Func2Comp[Comp <: Func2Comp[Comp, L, R], L <: DFAny, R <: DFAny]
(val leftArg: L, opString : String, val rightArg: R)(_width : Int) (
  implicit ctx: DFComponent.Context[Comp], cmp: DFAny.Companion
) extends DFComponent[Comp] with DSLSelfConnectedFoldableOwnerConstruct with DFAny {
  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  final protected[DFiant] val protComp: TCompanion = cmp.asInstanceOf[TCompanion]
  protected val tokenFunc : (L#TToken, R#TToken) => TToken

  final val inLeft = leftArg.copyAsNewPort(IN)
  final val inRight = rightArg.copyAsNewPort(IN)
  final val outResult = this.copyAsNewPort(OUT)

  private def initFunc: Seq[TToken] = {
    def leftInit = inLeft.getInit.asInstanceOf[Seq[leftArg.TToken]]
    def rightInit = inRight.getInit.asInstanceOf[Seq[rightArg.TToken]]
    DFAny.TokenSeq(leftInit, rightInit)(tokenFunc)
  }

  final lazy val protInit: Seq[TToken] = initFunc
  final lazy val constVal : TToken = tokenFunc(inLeft.constVal.asInstanceOf[leftArg.TToken], inRight.constVal.asInstanceOf[rightArg.TToken])

  inLeft.connectVal2Port(leftArg)
  inRight.connectVal2Port(rightArg)

  //  outResult.connectVal2Port(this)
  override def discoveryDepenencies: List[Discoverable] = super.discoveryDepenencies :+ outResult
  override protected def foldedRun: Unit = {
    outResult.setInitFunc.forced(initFunc)
  }

  final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  final val isPort = false

  override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
    if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
  override def constructCodeStringDefault: String = foldedConstructCodeString

  private[DFiant] override def designType : String = s"`Func2Comp$opString`"
  override def foldedConstructCodeString: String = s"${leftArg.refCodeString} $opString ${rightArg.refCodeString}"
  override def codeString: String = if (isFolded) super.codeString else valCodeString
}