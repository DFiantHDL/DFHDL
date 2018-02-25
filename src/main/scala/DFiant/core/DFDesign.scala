package DFiant.core

import DFiant.internals._

trait DFDesign extends DFInterface {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
}

abstract class DFBlackBox[Dsn <: DFDesign](implicit impl : DFBlackBox.Implementation[Dsn]) extends DFDesign {
  impl(this.asInstanceOf[Dsn])
}

object DFBlackBox {
  trait Implementation[Dsn <: DFDesign] {
    def apply(dsn : Dsn) : Unit
  }
}