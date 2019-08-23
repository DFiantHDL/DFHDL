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


trait Bug1 extends DFDesign {
  final val n = 32
  final val a   = DFBits(n) <> IN
  final val res = DFBits(n) <> OUT
  val tempL = DFBits(16)
  val tempR = DFBits(16)
  tempL := a(31, 16)
  tempR := a(15, 0)
  (tempL, tempR).bits.bits(23,16) := h"57"
  res(31, 16) := tempL
  res(15, 0) := tempR
}

//class IMem()(implicit ctx : RTComponent.Context) extends RTComponent {
//  final val clka = Clock()
//  final val addra = DFBits(12) <> IN
//  final val douta = DFBits(32) <> OUT
////  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
//}

trait Inst extends DFDesign {
  val g_predict = DFBool() <> OUT
  val l_predict = DFBool() <> OUT
  final val wea = DFBool() <> IN
  g_predict := wea
  l_predict := wea
}


class Cont()(implicit ctx : DFDesign.ContextOf[Cont]) extends DFDesign {
  val i = DFBool() <> IN
  val o = DFUInt(8) <> OUT
  val temp = DFUInt(8) init 0


  ifdf (i) {
    temp := temp
  }
//  temp := temp.prev(5)
//  temp.bits(3,0) := b"1111"
//  temp.bits(7,4) := b"0110"
  o := temp
}

trait Simy extends DFSimulator {
  val cont = new Cont()

}

object Bla extends DFApp {
  val bla = new Simy {}.printCodeString
  import internals._
//  println(bla.discoveredSet)
}
