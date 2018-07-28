package DFiant

import DFiant.basiclib._
import DFiant.internals._

object GlobalDesignName extends NameIt {
  override val value: String = "???"
}

object GlobalContext extends DFDesign.Context {
  override val owner: Option[DFBlock] = None
  override val basicLib: DFBasicLib = psuedoVendor.family.device.basicLib
  override val n: NameIt = GlobalDesignName
}

object GlobalDesign extends DFDesign()(GlobalContext) {
  override implicit val protChildOwner = this //Make it public for the global design
}
