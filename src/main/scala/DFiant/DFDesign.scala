package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFAnyOwner with Implicits {
  override protected implicit def protChildOwner : DFBlock = this
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
  protected def bodyCodeString : String = {
    val delim = "  "
    val noConst = discoveredList.filterNot(e => e.isInstanceOf[DFAny.Const])
    delim + noConst.codeString.replaceAll("\n","\n" + delim)
  }
  //final protected def discovery : Unit = protAlmanac

  final protected lazy val init : Unit = {
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  final val id = getID
}
object DFBlock {
  trait Context extends DFAnyOwner.ContextWithLib {
    val ifBlock: DFBlock
  }
  trait LowPriorityContext {
    implicit def evContext[Comp <: DFComponent[Comp]](
      implicit evContext : DFAnyOwner.ContextWithLib, evIfBlock : DFBlock = null, evNameIt : NameIt
    ) : Context = new Context {
      val owner: DFBlock = evContext.owner
      val ifBlock: DFBlock = evIfBlock
      val basicLib: DFBasicLib = evContext.basicLib
      val n: NameIt = evNameIt
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev (
      implicit evOwner : DFBlock = null, evIfBlock : DFBlock = null, evBasicLib : DFBasicLib, evNameIt : NameIt
    ) : Context = new Context {
      val owner: DFBlock = evOwner
      val ifBlock: DFBlock = evIfBlock
      val basicLib: DFBasicLib = evBasicLib
      val n: NameIt = evNameIt
    }
  }
}

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  override protected implicit def protChildOwner : DFDesign = this
  final override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  override def codeString: String = {
    s"val $name = new DFDesign {\n$bodyCodeString\n}"
  }
}

object DFDesign {
  type Context = DFBlock.Context
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp])
  extends DFDesign {
  ctx.impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFAnyOwner.ContextWithLib{
    val owner : DFBlock
    val impl : DFComponent.Implementation[Comp]
    val basicLib : DFBasicLib
    val n : NameIt
  }
  trait LowPriorityContext {
    implicit def ev2[Comp <: DFComponent[Comp]](
      implicit evContext : DFAnyOwner.ContextWithLib, evImpl : DFComponent.Implementation[Comp], evNameIt : NameIt
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
  object Implementation {
    type Context = DFAnyOwner.ContextWithLib
  }
}

