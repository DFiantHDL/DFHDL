package DFiant.FunctionalLib

import DFiant._
import DFiant.internals._
import singleton.twoface._

abstract class Selector[SW, W]
(val sel : DFUInt[SW])(val args: List[DFBits[W]]) (
  implicit ctx: DFComponent.Context[Selector[SW, W]], cmp: DFAny.Companion
) extends DFComponent[Selector[SW, W]] with DSLSelfConnectedFoldableOwnerConstruct with DFBits[W] {
  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](args.map(a => a.width.getValue).max)
  final protected[DFiant] val protComp: TCompanion = cmp.asInstanceOf[TCompanion]
  protected val tokenFunc : (DFUInt.Token, DFBits.Token) => DFBits.Token

  final val inSel = sel.copyAsNewPort(IN)
  final val inArgs = args.map(a => a.copyAsNewPort(IN))
  final val outResult = this.copyAsNewPort(OUT)

  private def initFunc: LazyBox[Seq[TToken]] = ??? // {
//    def leftInit = inLeft.getInit.asInstanceOf[Seq[leftArg.TToken]]
//    def rightInit = inRight.getInit.asInstanceOf[Seq[rightArg.TToken]]
//    DFAny.TokenSeq(leftInit, rightInit)(tokenFunc)
//  }

  final lazy val protInit: Seq[TToken] = initFunc.get
  final lazy val constLB : LazyBox[TToken] = ??? // tokenFunc(inLeft.constVal.asInstanceOf[leftArg.TToken], inRight.constVal.asInstanceOf[rightArg.TToken])

  inSel.connectVal2Port(sel)
  inArgs.zip(args).foreach{case (inArg, arg) => inArg.connectVal2Port(arg)}

  //  outResult.connectVal2Port(this)
  override def discoveryDepenencies: List[Discoverable] = super.discoveryDepenencies :+ outResult
  override protected def foldedRun: Unit = {
    outResult.setInitFunc.forced(initFunc)
  }

  final protected val foldedDiscoveryDependencyList = ??? //(outResult -> (inArgs :+ inSel)) :: Nil
  final val isPort = false

  override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
    if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
  override def constructCodeStringDefault: String = foldedConstructCodeString

  private[DFiant] override def designType : String = s"Selector"
  override def foldedConstructCodeString: String = ??? // s"${leftArg.refCodeString} $opString ${rightArg.refCodeString}"
  override def codeString: String = if (isFolded) super.codeString else valCodeString
}