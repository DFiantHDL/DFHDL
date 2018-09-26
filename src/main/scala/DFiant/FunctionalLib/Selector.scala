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

  final val inSel = new DFUInt.NewVar[SW](sel.width) <> IN
  final val inArgs = List.fill(args.length)(new DFBits.NewVar[Width](width) <> IN)
  final val outResult = new DFBits.NewVar[Width](width) <> OUT

  final protected[DFiant] val initLB : LazyBox[Seq[TToken]] =
    LazyBox.Args1List(this)(DFUInt.Token.select[DFBits.Token], inSel.initLB, inArgs.map(a => a.initLB))

  final protected[DFiant] lazy val constLB : LazyBox[TToken] =
    LazyBox.Args1List[DFBits.Token, DFUInt.Token, DFBits.Token](this)((a, l) => a.select(l), inSel.constLB, inArgs.map(a => a.constLB))

  inSel.connectVal2Port(sel)
  inArgs.zip(args).foreach{case (inArg, arg) => inArg.connectVal2Port(arg)}

  override def discoveryDepenencies: List[Discoverable] = super.discoveryDepenencies :+ outResult
  override protected def foldedRun: Unit = {
    outResult.setInitFunc.forced(initLB)
  }

  final protected val foldedDiscoveryDependencyList = (outResult -> (inArgs :+ inSel)) :: Nil
  final val isPort = false

  override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
    if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
  override def constructCodeStringDefault: String = foldedConstructCodeString

  private[DFiant] override def designType : String = s"Selector"
  override def foldedConstructCodeString: String = ??? // s"${leftArg.refCodeString} $opString ${rightArg.refCodeString}"
  override def codeString: String = if (isFolded) super.codeString else valCodeString
}