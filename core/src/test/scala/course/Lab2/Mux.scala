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

package course.Lab2

import DFiant._

trait Mux1 extends DFDesign {
  final val sel = DFBool() <> IN
  final val a   = DFBool() <> IN
  final val b   = DFBool() <> IN
  final val res = DFBool() <> OUT
  res := ((sel && a) || (!sel && b))
}

trait MuxN extends DFDesign {
  val w : Int
  final val sel = DFBool() <> IN
  final val a   = DFBits(w) <> IN
  final val b   = DFBits(w) <> IN
  final val res = DFBits(w) <> OUT
  for (i <- 0 until w) {
    val mux = new Mux1 {}.setName(s"m$i")
    mux.sel <> sel
    mux.a <> a(i)
    mux.b <> b(i)
    res(i) := mux.res
  }
}

object Mux {
  def apply[W](width : Int)(sel : DFBool)(trueVal : DFBits[W], falseVal : DFBits[W])(
    implicit ctx : DFAny.Op.Context
  ) : DFBits[Int] = {
    val mux = new MuxN {override lazy val w : Int = width}
    mux.sel <> sel
    mux.a <> trueVal
    mux.b <> falseVal
    mux.res
  }
}




