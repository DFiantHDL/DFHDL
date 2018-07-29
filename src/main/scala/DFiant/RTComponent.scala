package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx : RTComponent.Context) extends DFInterface {
  protected def newGeneric() : Unit = {}
  final val owner : DFBlock = ctx.owner

  final override protected def discoveryDepenencies : List[Discoverable] =
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isIn)
  final protected def discovery : Unit = {}

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
  final val id = getID
}

object RTComponent {
  type Context = DFAnyOwner.Context[DFBlock]
}