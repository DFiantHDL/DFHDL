package DFiant.core

import DFiant.internals._

trait DFDesign extends DFInterface {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
}

abstract class DFBlackBox[Ifc <: DFInterface](implicit impl : DFBlackBox.Implementation[Ifc]) extends DFInterface {
  impl(this.asInstanceOf[Ifc])
}

object DFBlackBox {
  abstract class Implementation[Ifc <: DFInterface](implicit dsn : DFDesign) {
    def apply(ifc : Ifc) : Unit
  }
}