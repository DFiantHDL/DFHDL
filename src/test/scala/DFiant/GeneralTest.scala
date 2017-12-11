package DFiant

import DFiant._
import DFiant.core._
import DFiant.internals._
import singleton.twoface._
import DFiant.fixedpoint._

import scala.annotation.implicitNotFound
/**
  * Created by soronpo on 12/26/16.
  */
import singleton.ops._
object GeneralTest {
//  import Addable._
//  import AdderBuilder._
//  val a = DFBits[4]; val b = a.msbits(4)

  foo(2)
  def foo[W](width : BitsWidth.Checked[W]) : Unit = {}

  foo(2)

//  val b = a + DFBits.Unsafe(3)
  val d = DFBits(2)
  d + d// Addable.DFBitsDFBits[d.Width, (d)
  val one = 100
  d + one
//  d + (d + d).wc

  val ff = DFBits[8]
//  ff.bitsWL[7,1]

  val ee = DFUFix(2,3)
//  val eew = ee.fw.width
//  implicitly[ee.Width =:= 5]


//  implicitly[e.width.T0 =:= 4]
//  var three = -3
//  val e : DFBits[4] = a ++ a// ++ a
//  a ++ a ++ a

}
  
//object FPL2017 {
//  type DFB = DFBits
//  abstract class Box(iT: DFB, iB: DFB) {
//    val oT: DFB
//    val oB: DFB
//  }
//  case class BoxY(iT: DFB, iB: DFB) extends Box(iT, iB) {
//    val (oT, oB) = (iT + iB, iB - iB)
//  }
//  case class BoxE(iT: DFB, iB: DFB) extends Box(iT, iB) {
//    val (oT, oB) = (iT - iB, iB + iB)
//  }
//  abstract class Box123(iT: DFB, iB: DFB) extends Box(iT, iB) {
//    def b1Bld(iT: DFB, iB: DFB) : Box
//    def b3Bld(iT: DFB, iB: DFB) : Box
//    val b1 = b1Bld(iT,     iB)
//    val b2 = BoxE(b1.oB,   b1.oT)
//    val b3 = b3Bld(b2.oB,  b2.oT)
//    val (oT, oB) = (b3.oT, b3.oB)
//  }
//  case class BoxYEE(iT: DFB, iB: DFB) extends Box123(iT, iB) {
//    def b1Bld(iT: DFB, iB: DFB) = BoxY(iT, iB)
//    def b3Bld(iT: DFB, iB: DFB) = BoxE(iT, iB)
//  }
//  case class BoxEEY(iT: DFB, iB: DFB) extends Box123(iT, iB) {
//    def b1Bld(iT: DFB, iB: DFB) = BoxE(iT, iB)
//    def b3Bld(iT: DFB, iB: DFB) = BoxY(iT, iB)
//  }
//  case class BoxBox(N: Int)(iT: DFB, iB: DFB) extends Box(iT, iB) {
//    val b = BoxY(iT, iB)
//    val bb : Box = if (N > 0) BoxBox(N - 1)(b.oT, b.oB)
//                   else b
//    val (oT, oB) = (bb.oT, bb.oB)
//  }
//}