package DFiant

import DFiant.internals._

trait DFDesign extends DFInterface with Implicits {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
}

object GlobalDesign extends DFDesign {
  override implicit val protDesign = this
}

abstract class DFComponent[Dsn <: DFDesign](implicit impl : DFComponent.Implementation[Dsn]) extends DFDesign {
  impl(this.asInstanceOf[Dsn])
}

object DFComponent {
  trait Implementation[Dsn <: DFDesign] {
    def apply(dsn : Dsn) : Unit
  }
}