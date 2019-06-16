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

import DFiant._

class PortArgDesign(val ti : DFUInt[8] <> IN, val to : DFUInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesign]) extends DFDesign {
  val padInt = new PortArgDesignInt(ti, to)
}

class PortArgDesignInt(val ti : DFUInt[8] <> IN, val to : DFUInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesignInt]) extends DFDesign {
  to <> ti + ti//.bits.uint
}

trait PortArgTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val temp = DFUInt(8)
  val io1 = new PortArgDesign(i, temp) {}
  val io2 = new PortArgDesign(temp, o) {}
}

object PortArgApp extends App {
  val paTest = new PortArgTest {}.printCodeString
}
