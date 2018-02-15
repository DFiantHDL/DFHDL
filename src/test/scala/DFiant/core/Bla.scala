package DFiant.core

import singleton.twoface._

abstract class Box[GenW]() extends DFDesign {
  val aa : DFUInt[8] <> IN = OPEN
  val a = DFBool()
  a && true
}

class RTComponent(name : String)

class Adder(leftWidth : Int, rightWidth : Int) extends DFDesign {
  trait Interface extends DFDesign.Interface {
    val left : DFUInt[Int] <> IN
    val right : DFUInt[Int] <> IN
    val result : DFUInt[Int] <> OUT
  }
  trait Instance extends DFDesign.Instance[Interface]
  trait Implementation extends DFDesign.Implementation[Interface]
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
