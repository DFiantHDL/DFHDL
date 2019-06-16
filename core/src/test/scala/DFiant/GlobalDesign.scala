/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import DFiant.BasicLib._
import DFiant.internals._

object GlobalDesignName extends NameIt {
  override val value: String = "GlobalDesign"
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
