package DFiant.FunctionalLib

import DFiant._
import singleton.twoface._

//Memory
abstract class Memory[W, D] private (width : TwoFace.Int[W], depth : TwoFace.Int[D])(initContents : Array[BitVector] = Array())(
  implicit ctx : DFComponent.Context[Memory[W, D]]
) extends DFComponent[Memory[W, D]] {
  final val sizeBits = width * depth
  trait Port {
    type AW
    val addrWidth : TwoFace.Int[AW] = ???
    final val addrToMem = new DFBits.NewVar[AW](addrWidth) <> IN
  }
  class ReadPort[RW](readWidth : TwoFace.Int[RW]) extends Port {
    final val dataFromMem = new DFBits.NewVar[RW](readWidth) <> OUT
  }
  class WritePort[WW](writeWidth : TwoFace.Int[WW]) extends Port {
    final val dataToMem = new DFBits.NewVar[WW](writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
  class ReadWritePort(readWidth : Int, writeWidth : Int) extends ReadPort(readWidth) {
    final val dataToMem = DFBits(writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
}

object Memory {
  def rom[W, D](width : Int, depth : Int)(fillFunc : BitVector => BitVector) : Memory[W, D] = ???
//  def ro2[W, D](width : Int, depth : Int)

}