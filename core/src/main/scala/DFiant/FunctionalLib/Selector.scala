//package DFiant.FunctionalLib
//
//import DFiant._
//import DFiant.internals._
//import singleton.twoface._
//
//abstract class Selector[SW, W]
//(val sel : DFUInt[SW])(val args: List[DFBits[W]]) (
//  implicit ctx: DFComponent.Context[Selector[SW, W]], cmp: DFAny.Companion = DFBits
//) extends DFComponent[Selector[SW, W]] with DSLSelfConnectedFoldableOwnerConstruct with DFBits[W] {
//  protected[DFiant] trait __Dev extends super.__DevDFComponent with super.__DevDFAny {
//
//  }
//  override private[DFiant] lazy val __dev : __Dev = new __Dev {}
//  import __dev._
//
//  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](args.map(a => a.width.getValue).max)
//
//  final val inSel = new DFUInt.NewVar[SW](sel.width) <> IN
//  final val inArgs = List.fill(args.length)(new DFBits.NewVar[Width](width) <> IN)
//  final val outResult = new DFBits.NewVar[Width](width) <> OUT
//
//  final protected[DFiant] val initLB : LazyBox[Seq[TToken]] =
//    LazyBox.Args1List(this)(DFUInt.Token.select[DFBits.Token], inSel.initLB, inArgs.map(a => a.initLB))
//
//  final protected[DFiant] lazy val constLB : LazyBox[TToken] =
//    LazyBox.Args1List[DFBits.Token, DFUInt.Token, DFBits.Token](this)((a, l) => a.select(l), inSel.constLB, inArgs.map(a => a.constLB))
//
//  override private[DFiant] lazy val inletSourceLB = ???
//
//  inSel.connectVal2Port(sel)
//  inArgs.zip(args).foreach{case (inArg, arg) => inArg.connectVal2Port(arg)}
//
//  override def discoveryDependencies: List[Discoverable] = super.discoveryDependencies :+ outResult
//  override protected def foldedRun: Unit = {
//    outResult.setInitFunc.forced(initLB)
//  }
//
//  final protected val foldedDiscoveryDependencyList = (outResult -> (inArgs :+ inSel)) :: Nil
//  final val isPort = false
//
//  override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
//    if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
//  override def constructCodeStringDefault: String = foldedConstructCodeString
//
//  private[DFiant] override def designType : String = s"Selector"
//  override def foldedConstructCodeString: String = ??? // s"${leftArg.refCodeString} $opString ${rightArg.refCodeString}"
//  override def codeString: String = if (isFolded) super.codeString else valCodeString
//}
//object Selector {
//  def apply[SW, W](sel : DFUInt[SW], default : => Option[DFBits[W]] = None)(args : List[DFBits[W]])(
//    implicit
//    ctx: DFComponent.Context[Selector[SW, W]]
//  ) : Selector[SW, W] = {
//    val maxLen = (2 << sel.width) - 1
//    val updatedArgs : List[DFBits[W]] =
//      if (maxLen < args.length) throw new IllegalArgumentException(s"\nSelector width is too small (${sel.width}) for the given list with ${args.length} elements")
//      else if (maxLen > args.length) { //Need to complete missing elements with the default parameter
//        val completeWith = default.getOrElse(throw new IllegalArgumentException(s"\nToo few elements in the list and no default argument provided"))
//        val completeCnt = maxLen - args.length
//        args ++ List.fill(completeCnt)(completeWith)
//      }
//      else args
//    new Selector[SW, W](sel)(updatedArgs) {}
//  }
//
//  implicit def ev[SW, W] : Selector[SW, W] => Unit = ifc => {
//    import ifc._
//  }
//}