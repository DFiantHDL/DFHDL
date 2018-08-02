package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFAnyOwner with Implicits {
  override implicit def theOwnerToBe : DFBlock = this
  final val owner = ctx.owner
  final implicit val basicLib = ctx.basicLib
  final implicit val config = ctx.config
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
  //final protected def discovery : Unit = protAlmanac

  final val id = getID
}
object DFBlock {
  trait ContextOf[T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner]
  object ContextOf {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit evOwner : Owner = null, evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      implicit val owner: Owner = evOwner
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  type Context = ContextOf[Nothing, DFBlock]
}

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  private var updatedOwner : DFDesign = this
  override implicit def theOwnerToBe : DFDesign = updatedOwner

  object ifdf {
    def genIf[IB <: DFDesign](ifBlock : IB, block: => Unit)(implicit ctx : DFIfBlock.Context) : IB = {
      val originalOwner = updatedOwner
      updatedOwner = ifBlock
      block
      updatedOwner = originalOwner
      ifBlock
    }
    def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFIfBlock.Context): DFIfBlock =
      genIf(new DFIfBlock(cond), block)

    protected class DFIfBlock(cond : DFBool)(implicit ctx : DFIfBlock.Context)
      extends DFDesign {
      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : DFIfBlock.Context)
      : DFIfBlock = genIf(new DFElseIfBlock(this, elseCond), elseBlock)
      def elsedf(elseBlock: => Unit)(implicit ctx : DFIfBlock.Context)
      : Unit = genIf(new DFElseBlock(this), elseBlock)

      override protected def createAlmanac : AlmanacIf = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ cond
      override def codeString: String =
        s"\nval $name = ifdf(${cond.name}) {$bodyCodeString\n}"
    }

    protected class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool)(implicit ctx : DFIfBlock.Context)
      extends DFIfBlock(cond) {
      override protected def createAlmanac : AlmanacElseIf =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
      override def codeString: String =
        s".elseifdf(${cond.name}) {$bodyCodeString\n}"
    }

    protected class DFElseBlock(prevIfBlock : DFIfBlock)(implicit ctx : DFIfBlock.Context)
      extends DFDesign {
      override protected def createAlmanac : AlmanacElse =
        new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
      override def codeString: String =
        s".elsedf {$bodyCodeString\n}"
    }

    object DFIfBlock {
      type Context = DFDesign.ContextOf[DFIfBlock]
    }

  }

  override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  override def codeString: String = {
    s"\nval $name = new DFDesign {$bodyCodeString\n}"
  }
}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context
  type ContextOf[T] = DFBlock.ContextOf[T, DFDesign]
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp])
  extends DFDesign {
  override protected def lateRun = ctx.impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Nothing, DFBlock] {
    implicit val impl : DFComponent.Implementation[Comp]
  }
  trait LowPriorityContext {
    implicit def ev2[Comp <: DFComponent[Comp]](
      implicit evContext : DFAnyOwner.ContextWithLib[DFBlock], evImpl : DFComponent.Implementation[Comp], evNameIt : NameIt
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evContext.owner
      implicit val impl: DFComponent.Implementation[Comp] = evImpl
      implicit val basicLib: DFBasicLib = evContext.basicLib
      implicit val config: DFAnyConfiguration = evContext.config
      val n: NameIt = evNameIt
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit evOwner : DFBlock, evImpl : DFComponent.Implementation[Comp], evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evOwner
      implicit val impl: DFComponent.Implementation[Comp] = evImpl
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }

  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
  object Implementation {
    type Context = DFAnyOwner.ContextWithLibOf[Implementation[_],DFBlock]
  }
}

