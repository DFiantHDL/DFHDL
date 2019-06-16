/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package Xilinx

object FPGAs {
  object `XC7VX485T-2FFG1761C` extends Device.XC7VX485T with SpeedGrade.`-2` with Package.FFG1761 with TempGrade.C {
    object IOPins {
      val R23 = Pin("IO_0_VRN_14",                "NA",   "14",   "0",    "HP")
      val A23 = Pin("IO_L1P_T0_D00_MOSI_14",      "0",    "14",   "0",    "HP")
      val A24 = Pin("IO_L1N_T0_D01_DIN_14",       "0",    "14",   "0",    "HP")
    }
  }
}
