package DFiant.FunctionalLib

import DFiant._
import internals._
import singleton.twoface._

abstract class Func2Comp[Comp <: Func2Comp[Comp, L, R], L <: DFAny, R <: DFAny]
(val leftArg: L, val opString : String, val rightArg: R)(_width : Int) (
  implicit ctx: DFComponent.Context[Comp], cmp: DFAny.Companion
) extends DFComponent[Comp] with DSLSelfConnectedFoldableOwnerConstruct with CanBePiped {
  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  final protected[DFiant] lazy val protComp: TCompanion = cmp.asInstanceOf[TCompanion]
  protected val tokenFunc : (L#TToken, R#TToken) => TToken

  final val leftLatency = LazyBox.Args1[Option[Int], DFAny.Source](this)(s => s.getMaxLatency, leftArg.flatSourceLB)
  final val rightLatency = LazyBox.Args1[Option[Int], DFAny.Source](this)(s => s.getMaxLatency, rightArg.flatSourceLB)
  final lazy val maxLatency = LazyBox.Args2[Option[Int], Option[Int], Option[Int]](this)((l, r) => List(l, r).max, leftLatency, rightLatency)
  override private[DFiant] def flatSourceLB : LazyBox[DFAny.Source] = {
    connect
    LazyBox.Args1[DFAny.Source, Option[Int]](this)(l => DFAny.Source.withLatency(this, l)/*.pipe(extraPipe)*/, maxLatency)
  }
  override private[DFiant] def foldedSourceLB : LazyBox[DFAny.Source] = {
    connect
    LazyBox.Args1[DFAny.Source, Option[Int]](this)(l => DFAny.Source.withLatency(this, l)/*.pipe(extraPipe)*/, maxLatency)
  }


  final val inLeft = leftArg.copyAsNewPort(IN)
  final val inRight = rightArg.copyAsNewPort(IN)
  final val outResult = this.copyAsNewPort(OUT)

  final protected[DFiant] lazy val initLB: LazyBox[Seq[TToken]] = {
    connect
    def leftInit = inLeft.initLB.asInstanceOf[LazyBox[Seq[leftArg.TToken]]]
    def rightInit = inRight.initLB.asInstanceOf[LazyBox[Seq[rightArg.TToken]]]
    LazyBox.Args2[Seq[TToken],Seq[leftArg.TToken],Seq[rightArg.TToken]](this)((l, r) => DFAny.TokenSeq(l, r)(tokenFunc), leftInit, rightInit)
  }

  final lazy val constLB : LazyBox[TToken] = LazyBox.Args2(this)(tokenFunc, inLeft.constLB.asInstanceOf[LazyBox[leftArg.TToken]], inRight.constLB.asInstanceOf[LazyBox[rightArg.TToken]])
//  private var extraPipe : Int = 0
//  def pipe() : this.type = pipe(1)
//  private[DFiant] override def pipeGet : Int = extraPipe
//  final def pipe(p : Int) : this.type = {extraPipe = p; this}
  final private[DFiant] lazy val leftBalancedSource = leftArg.thisSourceLB.get//.balanceTo(maxLatency.get)
  final private[DFiant] lazy val rightBalancedSource = rightArg.thisSourceLB.get//.getFoldedSource.balanceTo(maxLatency.get)

  lazy val connect : Unit ={
//    println(s"$fullName connected")
    inLeft.connectVal2Port(leftArg)
    inRight.connectVal2Port(rightArg)
  }
//  connect

  //  outResult.connectVal2Port(this)
  override def discoveryDepenencies: List[Discoverable] = super.discoveryDepenencies :+ outResult
  override protected def foldedRun: Unit = {
    outResult.setInitFunc.forced(initLB)
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

trait CompAlias extends CanBePiped {
  val comp : Func2Comp[_,_,_]
  lazy val unextendedLeft : DFAny = comp.leftArg.asInstanceOf[DFAny]
  final val alias = this.asInstanceOf[DFAny.Alias[_]]
  val bypassAlias : Boolean
//  def pipe() : this.type = pipe(1)
//  private[DFiant] override def pipeGet : Int = comp.pipeGet
//  def pipe(p : Int) : this.type = {comp.pipe(p); this}
}