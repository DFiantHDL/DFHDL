package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFAnyOwner with Implicits {
  final val owner = ctx.owner
  final implicit val basicLib = ctx.basicLib
  final val topDsn : DFDesign =
    if (owner != null) owner.topDsn
    else this.asInstanceOf[DFDesign] //The top will always be a DFDesign

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Sub-Blocks
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private lazy val blocks : List[DFBlock] = ownedList.collect{case o : DFBlock => o}
  final private lazy val rtcomponents : List[RTComponent] = ownedList.collect{case o : RTComponent => o}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final def isTop : Boolean = owner == null
  final override protected def nameDefault: String = if (isTop && ctx.n.value == "$anon") "top" else ctx.n.value
  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def discovery : Unit = protAlmanac

  final protected lazy val init : Unit = {
    //Run init of all rtcomponents
    rtcomponents.foreach(c => c.init)
    //Run init of all components
    blocks.foreach(c => c.init)
  }

  def codeString : String = {
//    print(mut)
    init
    discover
    protAlmanac.codeString
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  final val id = getID
}
object DFBlock {
  trait Context extends DFAnyOwner.ContextWithBasicLib[DFBlock]
  trait LowPriorityContext {
    implicit def evContext[Comp <: DFComponent[Comp]](implicit evCompCtx : DFComponent.Context[Comp], evNameIt : NameIt)
    : Context = new Context {
      val owner: DFBlock = evCompCtx.owner
      val basicLib: DFBasicLib = evCompCtx.basicLib
      val n: NameIt = evNameIt
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev(implicit evOwner : DFBlock = null, evBasicLib : DFBasicLib, evNameIt : NameIt)
    : Context = new Context {
      val owner: DFBlock = evOwner
      val basicLib: DFBasicLib = evBasicLib
      val n: NameIt = evNameIt
    }
  }
}

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  final override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies
}

object DFDesign {
  type Context = DFBlock.Context
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp])
  extends DFDesign {
  ctx.impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFAnyOwner.ContextWithBasicLib[DFBlock]{
    val owner : DFBlock
    val impl : DFComponent.Implementation[Comp]
    val basicLib : DFBasicLib
    val n : NameIt
  }
  trait LowPriorityContext {
    implicit def ev2[Comp <: DFComponent[Comp]](
      implicit evContext : DFAnyOwner.ContextWithBasicLib[DFBlock], evImpl : DFComponent.Implementation[Comp], evNameIt : NameIt
    ) : Context[Comp] = new Context[Comp] {
      val owner: DFBlock = evContext.owner
      val impl: DFComponent.Implementation[Comp] = evImpl
      val basicLib: DFBasicLib = evContext.basicLib
      val n: NameIt = evNameIt
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit evOwner : DFBlock, evImpl : DFComponent.Implementation[Comp], evBasicLib : DFBasicLib, evNameIt : NameIt
    ) : Context[Comp] = new Context[Comp] {
      val owner: DFBlock = evOwner
      val impl: DFComponent.Implementation[Comp] = evImpl
      val basicLib: DFBasicLib = evBasicLib
      val n: NameIt = evNameIt
    }
  }

  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

