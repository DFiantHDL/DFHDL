package DFiant.core

import singleton.twoface._

abstract class Box[GenW]() extends DFDesign {
  val aa : DFUInt[8] <> IN = OPEN
  val a = DFBool()
  a && true
}

class RTComponent(name : String)

trait AdderBuilder {
}
object Adder extends DFBlackBox {
  trait Interface extends DFBlackBox.Interface {
    type Left <: DFAny
    type Right <: DFAny
    type Result <: DFAny
    val left : Left <> IN
    val right : Right <> IN
    val result : Result <> OUT
  }
}


object Bla {
  import DFiant.GlobalDesign._
  val a = DFUInt(8)
  val b = DFUInt(8)
  val r = DFUInt(8)

  implicit def fro[LW, RW, OW] : DFBlackBox.Implementation[Adder.Interface {
    type Left = DFUInt[LW]
    type Right = DFUInt[RW]
    type Result = DFUInt[OW]
  }] = ???

  new Adder.Interface {
    type Left = DFUInt[8]
    type Right = DFUInt[8]
    type Result = DFUInt[8]
    val left = a
    val right = b
    val result = r
  }.instance
//  val inst8 = new Adder.Interface(8, 8) {
//    val left = a
//    val right = b
//    val result = r
//  }

//  Adder.addImplementation {ifc =>
//    ifc.result := ifc.left + ifc.right
//  }

}

class DFOpPlus[LW, RW](leftWidth : TwoFace.Int[LW], rightWidth : TwoFace.Int[RW]) extends DFDesign {
  trait IO {
    val left : DFUInt[LW] <> IN
    val right : DFUInt[RW] <> IN
    val result : DFUInt[LW] <> OUT
  }
  val io : IO = throw new UninitializedError()

  new RTComponent("+")
}
