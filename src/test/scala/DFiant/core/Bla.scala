package DFiant.core

import DFiant.core
import singleton.twoface._

abstract class Box[GenW]() extends DFDesign {
  val aa : DFUInt[8] <> IN = OPEN
  val a = DFBool()
  a && true
}

class RTComponent(name : String)

trait Adder[Left <: DFAny, Right <: DFAny, Result <: DFAny] extends DFBlackBox[Adder[Left, Right, Result]] {
  val left : Left <> IN
  val right : Right <> IN
  val result : Result <> OUT
}

object Adder {
  type DFU[LW, RW, OW] = Adder[DFUInt[LW], DFUInt[RW], DFUInt[OW]]
  implicit def fro[LW, RW, OW](implicit dsn : DFDesign) : DFBlackBox.Implementation[Adder.DFU[LW, RW, OW]] =
    new DFBlackBox.Implementation[Adder.DFU[LW, RW, OW]] {
      def apply(ifc : Adder.DFU[LW, RW, OW]) = {
        import ifc._
        result := left + right
      }
    }
}


object Bla {
  import DFiant.GlobalDesign._
  val a = DFUInt(8)
  val b = DFUInt(8)
  val r = DFUInt(8)

  new Adder[DFUInt[8], DFUInt[8], DFUInt[8]] {
    val left = a
    val right = b
    val result = r
  }
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
