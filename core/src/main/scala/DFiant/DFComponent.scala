package DFiant

import DFiant.BasicLib.DFBasicLib
import internals._

abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp], args : sourcecode.Args)
  extends DFDesign with DSLFoldableOwnerConstruct { self =>

  trait __DevDFComponent extends super.__DevDFDesign with super.__DevDSLFoldableOwner {

    override def postDiscoveryRun() : Unit = foldedDiscoveryDependencyList.collect {case Tuple2(out, inList) =>
      out.__dev.injectDependencies(inList)
      out.__dev.rediscoverDependencies()
    }

    override lazy val typeName: String = self.getClass.getSimpleName
  }
  override val __dev : __DevDFComponent = new __DevDFComponent {}

  def foldedConstructCodeString : String = {
    ctx.compName.value + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString
  }

  protected val foldedDiscoveryDependencyList : List[Tuple2[DFAny.Port[_ <: DFAny, _ <: OUT],List[DFAny.Port[_ <: DFAny, _ <: IN]]]]
  final override private[DFiant] def unfoldedRun = {
    ctx.impl(this.asInstanceOf[Comp])
    portsOut.foreach(p => p.__dev.rediscoverDependencies())
    isFolded = false
  }

  final protected def setInitFunc[DFVal <: DFAny.Initializable[_]](dfVal : DFVal)(value : LazyBox[Seq[dfVal.TToken]])
  : Unit = dfVal.setInitFunc.forced(value)
  final protected def getInit[DFVal <: DFAny.Initializable[_]](dfVal : DFVal) : LazyBox[Seq[dfVal.TToken]] = dfVal.initLB

  private[DFiant] override def constructCodeString : String = if (isFolded) foldedConstructCodeString else super.constructCodeString
  override def codeString : String = valCodeString

  final class InPortExtended(dfVal : DFAny.Port[_ <: DFAny, _ <: IN]) {
    def isOpen : Boolean = !dfVal.isConnected
  }
  final implicit def InPortExtended(dfVal: DFAny.Port[_ <: DFAny, _ <: IN]): InPortExtended = new InPortExtended(dfVal)
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Nothing, DFBlock] {
    implicit val impl : Comp => Unit
    val compName : sourcecode.Name.OfType[Comp]
  }
  trait LowPriority {
    implicit def evFromOpContext[Comp <: DFComponent[Comp]](
      implicit
      evContext : DFAny.Op.Context,
      evImpl : Comp => Unit,
      evNameIt : NameIt,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : NameIt.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val impl: Comp => Unit = evImpl
      implicit val basicLib: DFBasicLib = evContext.basicLib
      implicit val config: DFAnyConfiguration = evContext.config
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
  object Context extends LowPriority {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit
      evOwner : DFBlock,
      evImpl : Comp => Unit,
      evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : NameIt.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = Option(evOwner)
      implicit val impl: Comp => Unit = evImpl
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
}



