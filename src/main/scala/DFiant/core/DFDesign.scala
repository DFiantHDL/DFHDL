package DFiant.core

import DFiant.internals._

trait DFDesign extends DFPortShare {
  type Interface <: DFDesign.Interface

  def addImplementation(ifc : Interface => Unit) : Unit = {}
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
  trait Interface
}
