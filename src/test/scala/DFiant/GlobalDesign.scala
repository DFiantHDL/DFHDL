package DFiant

import DFiant.basiclib._

object DFGlobalLib extends DFBasicLib {
  implicit def `evU+U`[LW, RW, WCW](implicit dsn : DFDesign) : DFComponent.Implementation[`U+U`[LW, RW, WCW]] = ifc => {}
  implicit def `evU-U`[LW, RW, WCW](implicit dsn : DFDesign) : DFComponent.Implementation[`U-U`[LW, RW, WCW]] = ifc => {}
  implicit def `evU*U`[LW, RW, WCW](implicit dsn : DFDesign) : DFComponent.Implementation[`U*U`[LW, RW, WCW]] = ifc => {}

  implicit def `evU==U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U==U`[LW, RW]] = ifc => {}
  implicit def `evU!=U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U!=U`[LW, RW]] = ifc => {}
  implicit def `evU<U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U<U`[LW, RW]] = ifc => {}
  implicit def `evU>U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U>U`[LW, RW]] = ifc => {}
  implicit def `evU<=U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U<=U`[LW, RW]] = ifc => {}
  implicit def `evU>=U`[LW, RW](implicit dsn : DFDesign) : DFComponent.Implementation[`U>=U`[LW, RW]] = ifc => {}

  implicit def `evE==E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign)
  : DFComponent.Implementation[`E==E`[LE, RE]] = ifc => {
    import ifc._
  }
  implicit def `evE!=E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign)
  : DFComponent.Implementation[`E!=E`[LE, RE]] = ifc => {
    import ifc._
  }
}

object GlobalDesign extends DFDesign()(None, DFGlobalLib) {
  override implicit val dsn = this
}
