package DFiant

import DFiant.basiclib.DFBasicLib

object GlobalDesign extends DFDesign()(DFBasicLib) {
  override implicit val protDesign = this
}
