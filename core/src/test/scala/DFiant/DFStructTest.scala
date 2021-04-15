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

object Point extends DFStruct.Fields {
  val x = DFUInt(8) <> FIELD
  val y = DFUInt(8) <> FIELD
}

object Line extends DFStruct.Fields {
  val startPoint  = Point <> FIELD
  val endPoint    = Point <> FIELD
}

@df class LineWalk extends DFDesign {
  val LINE_SIZE = 5
  val token = Line.tokenGen {t => import t._
    startPoint := {t => import t._; x := 0        ; y := 0        }
    endPoint   := {t => import t._; x := LINE_SIZE; y := LINE_SIZE}
  }
  val line = Line <> OUT init token
  line.startPoint := line.endPoint.prev
  line.endPoint.x := line.endPoint.x.prev + LINE_SIZE
  line.endPoint.y := line.endPoint.y.prev + LINE_SIZE
}

@df object Active extends DFOpaque.Of((DFUInt(9), DFUInt(9)))
@df class Simple extends DFDesign {
  val a = (DFUInt(8), (DFBits(8), DFBits(8)), DFSInt(8)) <> IN init (0, (b0s, b1s), -3)
  val b = (DFUInt(8), (DFBits(8), DFBits(8)), DFSInt(8)) <> OUT
  val c = DFUInt(9) <> OUT
  val d = Active <> OUT init Active((5, 5))
  d := Active((5, 5))
  b <> a.prev
}


object TestSammy extends App {
  val top = new Simple
  import compiler._
  top.printCodeString.flattenTypes.printCodeString
}