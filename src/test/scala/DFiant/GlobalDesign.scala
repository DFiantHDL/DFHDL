package DFiant

import DFiant.basiclib._

object DFGlobalLib extends DFBasicLib {
  implicit def `ev+`[LW, RW, WCW] : DFComponent.Implementation[`U+U`[LW, RW, WCW]] = ifc => {}
  implicit def `ev-`[LW, RW, WCW] : DFComponent.Implementation[`U-U`[LW, RW, WCW]] = ifc => {}
  implicit def `ev*`[LW, RW, WCW] : DFComponent.Implementation[`U*U`[LW, RW, WCW]] = ifc => {}
}

object GlobalDesign extends DFDesign()(DFGlobalLib) {
  override implicit val dsn = this
}
