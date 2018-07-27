package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

protected abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFOwnerConstruct with Implicits {
  final val owner = ctx.owner
  final implicit val basicLib = ctx.basicLib
  final val topDsn : DFDesign = owner match {
    case Some(o) => o.topDsn
    case _ => this.asInstanceOf[DFDesign] //The top will always be a DFDesign
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Sub-Blocks
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val blocks : ListBuffer[DFBlock] = ListBuffer.empty[DFBlock]
  final private val rtcomponents : ListBuffer[RTComponent] = ListBuffer.empty[RTComponent]

  final private def newBlockGetID(comp : DFBlock) : Int = getNewID(blocks += comp)
  final private[DFiant] def newRTComponentGetID(comp : RTComponent) : Int = getNewID(rtcomponents += comp)

  final private def addBlockToOwnerGetID : Int = {
    owner match {
      case Some(o) => o.newBlockGetID(this)
      case _ => 0
    }
  }
  final protected def printBlocks() : Unit = {
    blocks.foreach(c => println(c.name))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final private[DFiant] def newDFValGetID(dfval : DFAny) : Int = getNewID(dfvals += dfval)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final def isTop : Boolean = owner.isEmpty
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
    init
    discover
    protAlmanac.codeString
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  final val id = addBlockToOwnerGetID
}
object DFBlock {
  trait Context {
    val owner : Option[DFBlock]
    val basicLib : DFBasicLib
    val n : NameIt
  }
  object Context {
    implicit def ev(implicit evOwner : Option[DFBlock] = None, evBasicLib : DFBasicLib, evNameIt : NameIt)
    : Context = new Context {
      val owner: Option[DFBlock] = evOwner
      val basicLib: DFBasicLib = evBasicLib
      val n: NameIt = evNameIt
    }
    implicit def ev2(implicit evDesignContext : DFDesign.Context)
    : Context = new Context {
      val owner: Option[DFBlock] = evDesignContext.owner
      val basicLib: DFBasicLib = evDesignContext.basicLib
      val n: NameIt = evDesignContext.n
    }
  }
}

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  final override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies
}
object DFDesign {
  trait Context {
    val owner : Option[DFBlock]
    val basicLib : DFBasicLib
    val n : NameIt
  }
  object Context {
    implicit def ev(implicit evOwner : Option[DFBlock] = None, evBasicLib : DFBasicLib, evNameIt : NameIt)
    : Context = new Context {
      val owner: Option[DFBlock] = evOwner
      val basicLib: DFBasicLib = evBasicLib
      val n: NameIt = evNameIt
    }
    implicit def ev2[Comp <: DFComponent[Comp]](implicit evCompCtx : DFComponent.Context[Comp])
    : Context = new Context {
      val owner: Option[DFBlock] = Some(evCompCtx.owner)
      val basicLib: DFBasicLib = evCompCtx.basicLib
      val n: NameIt = evCompCtx.n
    }
  }
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp])
  extends DFDesign {
  ctx.impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] {
    val owner : DFBlock
    val impl : DFComponent.Implementation[Comp]
    val basicLib : DFBasicLib
    val n : NameIt
  }
  object Context {
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

