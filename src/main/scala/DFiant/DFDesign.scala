package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.HashMap

abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFAnyOwner with Implicits {
  override implicit def theOwnerToBe : DFBlock = this
  final val owner = ctx.owner
  final implicit val basicLib = ctx.basicLib
  final implicit val config = ctx.config
  final val topDsn : DFDesign =
    if (owner != null) owner.topDsn
    else this.asInstanceOf[DFDesign] //The top will always be a DFDesign
  private[DFiant] val designSet : HashMap[String, Int] =
    if (owner == null) HashMap.empty[String, Int] else owner.designSet

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Sub-Blocks
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private lazy val blocks : List[DFBlock] = memberList.collect{case o : DFBlock => o}
  final private lazy val rtcomponents : List[RTComponent] = memberList.collect{case o : RTComponent => o}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final def isTop : Boolean = owner == null
  override private[DFiant] def nameDefault: String = ctx.n.value
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //final protected def discovery : Unit = protAlmanac

  final val id = getID
}
object DFBlock {
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner]
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

protected[DFiant] trait ConditionalBlock

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  private var updatedOwner : DFDesign = this
  final override implicit def theOwnerToBe : DFDesign = updatedOwner

  final object ifdf {
    private def genIf[IB <: DFDesign](ifBlock : IB, block: => Unit)(implicit ctx : DFIfBlock.Context) : IB = {
      val originalOwner = updatedOwner
      updatedOwner = ifBlock
      block
      updatedOwner = originalOwner
      ifBlock
    }
    def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFIfBlock.Context): DFIfBlock =
      genIf(new DFIfBlock(cond), block)

    protected class DFIfBlock(cond : DFBool)(implicit ctx : DFIfBlock.Context)
      extends DFDesign with ConditionalBlock {
      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : DFIfBlock.Context)
      : DFIfBlock = genIf(new DFElseIfBlock(this, elseCond), elseBlock)
      def elsedf(elseBlock: => Unit)(implicit ctx : DFIfBlock.Context)
      : Unit = genIf(new DFElseBlock(this), elseBlock)

      override private[DFiant] def createAlmanac : AlmanacIf = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ cond
      override private[DFiant] def nameDefault: String = ctx.n.value
      override def codeString: String = s"\nval $name = ifdf(${cond.name}) {$bodyCodeString\n}"
    }

    protected class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool)(implicit ctx : DFIfBlock.Context)
      extends DFIfBlock(cond) {
      override private[DFiant] def nameDefault: String = ctx.n.value + "$elseif"
      override private[DFiant] def createAlmanac : AlmanacElseIf =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
      override def codeString: String = s".elseifdf(${cond.name}) {$bodyCodeString\n}"
    }

    protected class DFElseBlock(prevIfBlock : DFIfBlock)(implicit ctx : DFIfBlock.Context)
      extends DFDesign with ConditionalBlock {
      override private[DFiant] def nameDefault: String = ctx.n.value + "$else"
      override private[DFiant] def createAlmanac : AlmanacElse =
        new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
    }

    protected object DFIfBlock {
      type Context = DFDesign.ContextOf[DFIfBlock]
    }

  }

  def constructCodeString : String = s"new DFDesign {$commentClassName$bodyCodeString\n}"
  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

  override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  private def commentClassName : String = if (ctx.config.commentClassNames) s"  //$typeName" else ""
  override def codeString: String = s"\nval $name = $constructCodeString"

}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context
  type ContextOf[+T] = DFBlock.ContextOf[T, DFDesign]
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp])
  extends DFDesign with DSLFoldedOwnerConstruct {
  def foldedConstructCodeString : String = super.constructCodeString
  final override private[DFiant] lazy val unfold = {
    ctx.impl(this.asInstanceOf[Comp])
    folded = false
  }
  override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ portsIn //TODO: should be changed so that any DFComponent can set its own port dependencies
  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : () => Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  final override def constructCodeString : String = if (folded) foldedConstructCodeString else super.constructCodeString
  final override def codeString : String = super.codeString

  final class InPortExtended(dfVal : DFAny.Port[_ <: DFAny, _ <: IN]) {
    def isOpen : Boolean = dfVal.connectedSource.isEmpty
  }
  final implicit def InPortExtended(dfVal: DFAny.Port[_ <: DFAny, _ <: IN]): InPortExtended = new InPortExtended(dfVal)
//  override lazy val typeName: String = getClass.getSimpleName
}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Nothing, DFBlock] {
    implicit val impl : DFComponent.Implementation[Comp]
  }
  object Context {
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

