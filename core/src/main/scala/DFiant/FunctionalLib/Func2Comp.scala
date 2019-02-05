package DFiant.FunctionalLib

import DFiant._
import internals._
import singleton.twoface._

abstract class Func2Comp[Comp <: Func2Comp[Comp, L, R], L <: DFAny, R <: DFAny]
(val leftArg: L, val opString : String, val rightArg: R)(_width : Int) (
  implicit ctx: DFComponent.Context[Comp], cmp: DFAny.Companion
) extends DFComponent[Comp] with DSLSelfConnectedFoldableOwnerConstruct with CanBePiped {self =>

  type TDev <: __Dev
  protected[DFiant] trait __Dev extends super[DFComponent].__DevDFComponent with super[CanBePiped].__Dev {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
      if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
    override def constructCodeStringDefault: String = foldedConstructCodeString
    override protected def designType : String = s"`Func2Comp$opString`"
    override def foldedConstructCodeString: String = s"${leftBalancedSource.refCodeString} $opString ${rightBalancedSource.refCodeString}"
    override def codeString: String = if (isFolded) super.codeString else valCodeString

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override def discoveryDependencies: List[Discoverable] = super.discoveryDependencies :+ outResult :+ leftArg :+ rightArg

    override def inletSourceLB : LazyBox[Source] = {
      LazyBox.Args1[Source, Option[Int]](self)(l => Source.withLatency(self, l)/*.pipe(extraPipe)*/, maxLatency)
    }

    final lazy val initLB: LazyBox[Seq[TToken]] = {
      def leftInit = inLeft.initLB.asInstanceOf[LazyBox[Seq[leftArg.TToken]]]
      def rightInit = inRight.initLB.asInstanceOf[LazyBox[Seq[rightArg.TToken]]]
      LazyBox.Args2[Seq[TToken],Seq[leftArg.TToken],Seq[rightArg.TToken]](self)((l, r) => DFAny.TokenSeq(l, r)(tokenFunc), leftInit, rightInit)
    }

    final lazy val constLB : LazyBox[TToken] = LazyBox.Args2(self)(tokenFunc, inLeft.constLB.asInstanceOf[LazyBox[leftArg.TToken]], inRight.constLB.asInstanceOf[LazyBox[rightArg.TToken]])
    //  private var extraPipe : Int = 0
    //  def pipe() : this.type = pipe(1)
    //  private[DFiant] override def pipeGet : Int = extraPipe
    //  final def pipe(p : Int) : this.type = {extraPipe = p; this}
    final lazy val leftBalancedSource = leftArg.thisSourceLB.get.balanceTo(maxLatency.get)
    final lazy val rightBalancedSource = rightArg.thisSourceLB.get.balanceTo(maxLatency.get)
  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  __dev
  import __dev._

  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  protected val tokenFunc : (L#TToken, R#TToken) => TToken

  final val leftLatency = LazyBox.Args1[Option[Int], Source](this)(s => s.getMaxLatency, leftArg.thisSourceLB)
  final val rightLatency = LazyBox.Args1[Option[Int], Source](this)(s => s.getMaxLatency, rightArg.thisSourceLB)
  final lazy val maxLatency = LazyBox.Args2[Option[Int], Option[Int], Option[Int]](this)((l, r) => List(l, r).max, leftLatency, rightLatency)

  final val inLeft = leftArg.copyAsNewPort(IN)
  final val inRight = rightArg.copyAsNewPort(IN)
  final val outResult = this.copyAsNewPort(OUT)

  atOwnerDo {
    inLeft.connectVal2Port(leftArg)
    inRight.connectVal2Port(rightArg)
//    outResult.connectVal2Port(this)
  }

  //TODO: leftArg, rightArg
  override protected def foldedRun: Unit = {
    outResult.setInitFunc.forced(initLB)
  }

  final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  final val isPort = false
}
object Func2Comp {
  implicit def fetchDev(from : Func2Comp[_,_,_])(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
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