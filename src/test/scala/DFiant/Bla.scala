package DFiant

import singleton.twoface._
import psuedoVendor.family.device._

trait Foo extends DFDesign {
  type W = 8
  val left = DFUInt[W] <> IN
  val right = DFUInt[W] <> IN
  val result = DFUInt[W] <> OUT

  result <> left + right
}

//trait Adder[Left <: DFAny, Right <: DFAny, Result <: DFAny] extends DFComponent[Adder[Left, Right, Result]] {
//  val left : Left <> IN
//  val right : Right <> IN
//  val result : Result <> OUT
//}
//
//object Adder {
//  type DFU[LW, RW, OW] = Adder[DFUInt[LW], DFUInt[RW], DFUInt[OW]]
//  implicit def fro[LW, RW, OW] : DFComponent.Implementation[Adder.DFU[LW, RW, OW]] = ifc => {
//    import ifc._
//    result <> left + right
//  }
//}


//object Bla {
////  trait Comp extends DFDesign {
////    val i : DFUInt[8] <> IN = OPEN
////    val o : DFUInt[8] <> OUT = OPEN
////    o := i
////  }
////
////  val L = new Comp {}
////  val R = new Comp {}
////
////  L.i <> R.o
////  L.o <> R.i
//
//  new DFDesign {
//    val a = DFUInt(7).extendable
//    val b = DFUInt(8)
//    val r = DFUInt(8)
//
//    type E = 8
//    new Adder[DFUInt[E], DFUInt[E], DFUInt[E]] {
//      val left = a
//      val right = 20L
//      val result = r
//    }
//  }
//}
//
//class DFOpPlus[LW, RW](leftWidth : TwoFace.Int[LW], rightWidth : TwoFace.Int[RW]) extends DFDesign {
//
//  trait IO {
//    val left: DFUInt[LW] <> IN
//    val right: DFUInt[RW] <> IN
//    val result: DFUInt[LW] <> OUT
//  }
//
//  val io: IO = throw new UninitializedError()
//}
