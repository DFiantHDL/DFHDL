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
object Adder extends DFDesign {
  abstract class Interface(leftWidth : Int, rightWidth : Int) extends DFDesign.Interface {
    val left : DFUInt[Int] <> IN
    val right : DFUInt[Int] <> IN
    val result : DFUInt[Int] <> OUT
  }
}


object Bla {
  import DFiant.GlobalDesign._
  val a = DFUInt(8)
  val b = DFUInt(8)
  val r = DFUInt(8)
  val inst8 = new Adder.Interface(8, 8) {
    val left = a
    val right = b
    val result = r
  }

  Adder.addImplementation {ifc =>
    ifc.result := ifc.left + ifc.right
  }

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
