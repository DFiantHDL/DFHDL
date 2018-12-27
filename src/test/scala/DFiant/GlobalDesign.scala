package DFiant

import DFiant.BasicLib._
import DFiant.internals._

object GlobalDesignName extends NameIt {
  override val value: String = "GlobalDesign"
  override val owner: String = s"${Name.AnonStart}anon"
}

object GlobalContext extends DFDesign.Context {
  override val ownerOption : Option[DFBlock] = None
  override val basicLib: DFBasicLib = Xilinx.FPGAs.`XC7VX485T-2FFG1761C`.basicLib
  override val config: DFAnyConfiguration = DFAnyConfiguration.default
  override val n: NameIt = GlobalDesignName
}

object GlobalDesign extends DFDesign()(GlobalContext) {
  implicit val publicOwner = this //Make it public for the global design
}
