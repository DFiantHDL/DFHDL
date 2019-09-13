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

package fpga2019
import DFiant._

trait Box extends DFDesign {
  val iT = DFSInt[16] <> IN
  val iB = DFSInt[16] <> IN
  val oT = DFSInt[16] <> OUT
  val oB = DFSInt[16] <> OUT
  iT.prev <> oT
  oB := iB.prev
}

trait BoxTop extends DFDesign {
  val iT = DFSInt[16] <> IN init (5, 7)
  val iB = DFSInt[16] <> IN init (2, 6)
  val oT = DFSInt[16] <> OUT
  val oB = DFSInt[16] <> OUT
  val boxL = new Box {}
  val boxR = new Box {}
  boxL.iT <> iT
  boxL.iB <> iB
  boxL.oT <> boxR.iT
  boxL.oB <> boxR.iB
  boxR.oT <> oT
  boxR.oB <> oB
}

object BoxTopApp extends DFApp.VHDLCompiler[BoxTop]
