package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

abstract class DFDesign(implicit val basicLib: DFBasicLib) extends DFInterface with Implicits {
  protected implicit val dsn = this
  protected[DFiant] val protAlmanac = new Almanac {}
  protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign {
}

abstract class DFComponent[Comp <: DFComponent[Comp]](implicit impl : DFComponent.Implementation[Comp], basicLib: DFBasicLib) extends DFDesign {
  impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

abstract class RTComponent(implicit dsn : DFDesign) extends DFInterface {
  dsn.addRTComponent(this)
}