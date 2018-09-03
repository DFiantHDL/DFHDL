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
    folded = false
  }

  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : => Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  final private[DFiant] override def constructCodeString : String = if (config.foldComponents) foldedConstructCodeString else super.constructCodeString
  final override def codeString : String = valCodeString

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


//abstract class DiSoComp[Kind, LW, RW, OW](left : DFUInt[LW], right : DFUInt[RW], width : TwoFace.Int[OW])
//  (func : (DFUInt.Token, DFUInt.Token) => DFUInt.Token)(
//  implicit ctx : DFComponent.Context[DiSoComp[Kind, LW, RW, OW]]
//) extends DFComponent[DiSoComp[Kind, LW, RW, OW]] with DFUInt[OW] {
//  final val inLeft = new DFUInt.NewVar[LW](left.width) <> IN
//  final val inRight = new DFUInt.NewVar[RW](right.width) <> IN
//  final val outResult = new DFUInt.NewVar[OW](width) <> OUT
//  inLeft.connectVal2Port(left)(ctx.updateOwner(ctx.owner))
//  inRight.connectVal2Port(right)(ctx.updateOwner(ctx.owner))
//}
