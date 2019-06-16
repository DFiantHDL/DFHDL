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

package DFiant.FunctionalLib

import DFiant._
import singleton.ops._
import singleton.twoface._
import internals._

//Memory
abstract class Memory[W, D] private (width : TwoFace.Int[W], depth : TwoFace.Int[D])(initContents : Array[BitVector] = Array())(
  implicit ctx : DFComponent.Context[Memory[W, D]]
) extends DFComponent[Memory[W, D]] {
  final val sizeBits = width * depth
  trait Port {
    type AW
    val addrWidth : TwoFace.Int[AW]
    final val addrToMem = new DFBits.NewVar[AW](addrWidth) <> IN
  }
  abstract class ReadPort[RW](readWidth : TwoFace.Int[RW]) extends Port {
    final val dataFromMem = new DFBits.NewVar[RW](readWidth) <> OUT
  }
  object ReadPort {
    type Aux[RW, AW0] = ReadPort[RW] {type AW = AW0}
    type Inference[RW] = BitsWidthOf.Int[(W * D) / RW]
    def apply[RW](readWidth: BitsWidth.Checked[RW])(implicit calcAddrWidth: Inference[RW])
    : Aux[RW, calcAddrWidth.Out] = new ReadPort[RW](readWidth) {
      type AW = calcAddrWidth.Out
      val addrWidth: TwoFace.Int[AW] = calcAddrWidth((width * depth) / readWidth)
    }
  }
  abstract class WritePort[WW](writeWidth : TwoFace.Int[WW]) extends Port {
    final val dataToMem = new DFBits.NewVar[WW](writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
  abstract class ReadWritePort(readWidth : Int, writeWidth : Int) extends ReadPort(readWidth) {
    final val dataToMem = DFBits(writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
}

object Memory {
  def rom[W, D](width : Int, depth : Int)(fillFunc : BitVector => BitVector) : Memory[W, D] = ???
//  def ro2[W, D](width : Int, depth : Int)
}

