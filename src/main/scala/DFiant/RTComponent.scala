package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit dsn : DFDesign, n : NameIt) extends DFInterface {
  protected def newGeneric() : Unit = {}

  final val id = dsn.newRTComponentGetID(this)
}