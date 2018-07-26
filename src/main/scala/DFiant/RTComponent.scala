package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit blk : DFBlock, n : NameIt) extends DFInterface {
  protected def newGeneric() : Unit = {}

  final protected def discoveryDepenencies : List[Discoverable] =
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isIn)
  final protected def discovery : Unit = {}

  final val id = blk.newRTComponentGetID(this)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override protected def nameDefault: String = n.value
  final lazy val fullName : String = s"${blk.fullName}.$name"
  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected[DFiant] lazy val init : Unit = {
    //set Output Ports Dependency
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isOut).foreach(p => p.setComponentDependency(this))
  }

}