package DFiant.core

import DFiant.internals._

trait DFDesign extends DFPortShare {
  type Interface <: DFDesign.Interface
  type Instance <: DFDesign.Instance[Interface]
  type Implementation <: DFDesign.Implementation[Interface]

  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
  trait Interface
  trait Implementation[Ifc <: Interface] {
    val io : Ifc
  }
  trait Instance[Ifc <: Interface] {

  }
}
