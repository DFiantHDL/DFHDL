package DFiant.core

import DFiant.internals._

trait DFDesign extends DFInterface {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
}

abstract class DFBlackBox extends DFDesign {
  type Interface <: DFBlackBox.Interface
}
object DFBlackBox {
  trait Implementation[Ifc <: Interface] {
    def apply(ifc : Ifc)
  }
  trait Interface extends DFInterface {
    def instance[Ifc <: Interface](implicit impl : Implementation[Ifc]) = impl(this.asInstanceOf[Ifc])
  }
}