package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx : DFAny.Op.Context) extends DFInterface {
  protected def newGeneric() : Unit = {}
  final val owner : DFBlock = ctx.owner

  final override protected def discoveryDepenencies : List[Discoverable] =
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isIn)
  final protected def discovery : Unit = {}

  final val id = ctx.owner.newRTComponentGetID(this)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override protected def nameDefault: String = ctx.n.value
  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected[DFiant] lazy val init : Unit = {
    //set Output Ports Dependency
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isOut).foreach(p => p.setComponentDependency(this))
  }

}

object RTComponent {
  trait Context {
    val owner : DFBlock
    val n : NameIt
  }
  trait LowPriorityContext {
    implicit def ev2[Comp <: DFComponent[Comp]](implicit evCompCtx : DFComponent.Context[Comp])
    : Context = new Context {
      val owner: DFBlock = evCompCtx.owner
      val n: NameIt = evCompCtx.n
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev(implicit evOwner : DFBlock, evNameIt : NameIt)
    : Context = new Context {
      val owner: DFBlock = evOwner
      val n: NameIt = evNameIt
    }
  }
}