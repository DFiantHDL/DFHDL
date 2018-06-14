package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit dsn : DFDesign, n : NameIt) extends DFInterface with Discoverable {
  protected def newGeneric() : Unit = {}

  final protected def discoveryDepenencies : List[Discoverable] =
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isIn)
  final protected def discovery : Unit = {}

  final val id = dsn.newRTComponentGetID(this)

  final protected[DFiant] lazy val init : Unit = {
    //set Output Ports Dependency
    portNodes.map(pn => pn.dfport).filter(p => p.dir.isOut).foreach(p => p.setComponentDependency(this))
  }

}