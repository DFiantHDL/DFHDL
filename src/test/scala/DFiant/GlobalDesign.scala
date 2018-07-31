package DFiant

import DFiant.basiclib._
import DFiant.internals._

object GlobalDesignName extends NameIt {
  override val value: String = "???"
  override val isAnonymous: Boolean = true
}

object GlobalContext extends DFDesign.Context {
  override val owner: DFBlock = null
  override val basicLib: DFBasicLib = psuedoVendor.family.device.basicLib
  override val n: NameIt = GlobalDesignName
}

object GlobalDesign extends DFDesign()(GlobalContext) {
  implicit val publicOwner = this //Make it public for the global design
}
