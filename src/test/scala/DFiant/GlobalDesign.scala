package DFiant

import DFiant.basiclib._
import DFiant.internals._

object DFGlobalLib extends DFBasicLib {
  import DFComponent.Implementation
  implicit def `evU+U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U+U`[LW, RW, WCW]] = ifc => {}
  implicit def `evU-U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U-U`[LW, RW, WCW]] = ifc => {}
  implicit def `evU*U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U*U`[LW, RW, WCW]] = ifc => {}

  implicit def `evU==U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U==U`[LW, RW]] = ifc => {}
  implicit def `evU!=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U!=U`[LW, RW]] = ifc => {}
  implicit def `evU<U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<U`[LW, RW]] = ifc => {}
  implicit def `evU>U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>U`[LW, RW]] = ifc => {}
  implicit def `evU<=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<=U`[LW, RW]] = ifc => {}
  implicit def `evU>=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>=U`[LW, RW]] = ifc => {}

  implicit def `evE==E`[E <: Enum](implicit dsn : DFDesign) : Implementation[`E==E`[E]] = ifc => {}
  implicit def `evE!=E`[E <: Enum](implicit dsn : DFDesign) : Implementation[`E!=E`[E]] = ifc => {}
}

object GlobalDesignName extends NameIt {
  override val value: String = "???"
}

object GlobalDesign extends DFDesign()(None, DFGlobalLib, GlobalDesignName) {
  override implicit val dsn = this
  def implementation(): Unit = {}
}
