package DFiant

import DFiant.basiclib._

object DFGlobalLib extends DFBasicLib {
  implicit def `ev+`[LW, RW, WCW] : DFComponent.Implementation[`U+U`[LW, RW, WCW]] = ifc => {}
  implicit def `ev-`[LW, RW, WCW] : DFComponent.Implementation[`U-U`[LW, RW, WCW]] = ifc => {}
  implicit def `ev*`[LW, RW, WCW] : DFComponent.Implementation[`U*U`[LW, RW, WCW]] = ifc => {}

  implicit def `ev==`[LW, RW] : DFComponent.Implementation[`U==U`[LW, RW]] = ifc => {}
  implicit def `ev!=`[LW, RW] : DFComponent.Implementation[`U!=U`[LW, RW]] = ifc => {}
  implicit def `ev<`[LW, RW] : DFComponent.Implementation[`U<U`[LW, RW]] = ifc => {}
  implicit def `ev>`[LW, RW] : DFComponent.Implementation[`U>U`[LW, RW]] = ifc => {}
  implicit def `ev<=`[LW, RW] : DFComponent.Implementation[`U<=U`[LW, RW]] = ifc => {}
  implicit def `ev>=`[LW, RW] : DFComponent.Implementation[`U>=U`[LW, RW]] = ifc => {}
}

object GlobalDesign extends DFDesign()(None, DFGlobalLib) {
  override implicit val dsn = this
}
