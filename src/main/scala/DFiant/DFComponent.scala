package DFiant

import DFiant.BasicLib.DFBasicLib
import internals._
import singleton.twoface._

abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp], args : sourcecode.Args)
  extends DFDesign with DSLFoldableOwnerConstruct {
  def foldedConstructCodeString : String = {
    ctx.compName.value + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString
  }

  protected val foldedDiscoveryDependencyList : List[Tuple2[DFAny.Port[_ <: DFAny, _ <: OUT],List[DFAny.Port[_ <: DFAny, _ <: IN]]]]
  final override private[DFiant] def unfoldedRun = {
    ctx.impl(this.asInstanceOf[Comp])
    portsOut.foreach(p => p.rediscoverDependencies)
    isFolded = false
  }

  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : => Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  private[DFiant] override def constructCodeString : String = if (isFolded) foldedConstructCodeString else super.constructCodeString
  override def codeString : String = valCodeString

  final class InPortExtended(dfVal : DFAny.Port[_ <: DFAny, _ <: IN]) {
    def isOpen : Boolean = dfVal.connectedSource.isEmpty
  }
  final implicit def InPortExtended(dfVal: DFAny.Port[_ <: DFAny, _ <: IN]): InPortExtended = new InPortExtended(dfVal)
  override lazy val typeName: String = getClass.getSimpleName

  override def postDiscoveryRun : Unit = foldedDiscoveryDependencyList.collect {case Tuple2(out, inList) =>
    out.injectDependencies(inList)
    out.rediscoverDependencies
  }
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Nothing, DFBlock] {
    implicit val impl : Comp => Unit
    val compName : sourcecode.Name.OfType[Comp]
  }
  trait LowPriority {
    implicit def evFromOpContext[Comp <: DFComponent[Comp]](
      implicit evContext : DFAny.Op.Context, evImpl : Comp => Unit,
      evNameIt : NameIt, evCompName : sourcecode.Name.OfType[Comp]
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evContext.owner
      implicit val impl: Comp => Unit = evImpl
      implicit val basicLib: DFBasicLib = evContext.basicLib
      implicit val config: DFAnyConfiguration = evContext.config
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
  object Context extends LowPriority {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit evOwner : DFBlock, evImpl : Comp => Unit, evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration, evNameIt : NameIt, evCompName : sourcecode.Name.OfType[Comp]
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evOwner
      implicit val impl: Comp => Unit = evImpl
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
}


abstract class Func2Comp[Comp <: Func2Comp[Comp, L, R], L <: DFAny, R <: DFAny]
  (val leftArg: L, opString : String, val rightArg: R)(_width : Int) (
  implicit ctx: DFComponent.Context[Comp], cmp: DFAny.Companion
) extends DFComponent[Comp] with DFAny {
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
  override def foldedConstructCodeString: String = {
    val leftCodeString = leftArg.refCodeString
    val rightCodeString = rightArg.refCodeString
    //TODO: fix this space hack
    val bracketLeft = if (leftCodeString.contains(" ")) s"($leftCodeString)" else leftCodeString
    val bracketRight = if (rightCodeString.contains(" ")) s"($rightCodeString)" else rightCodeString
    s"$bracketLeft $opString $bracketRight"
  }
  override def codeString: String = if (isFolded) super.codeString else valCodeString
}
