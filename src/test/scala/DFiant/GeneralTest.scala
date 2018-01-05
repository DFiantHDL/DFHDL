package DFiant

import DFiant._
import DFiant.core._
import DFiant.internals._
import singleton.twoface._
import DFiant.tokens._
//import DFiant.fixedpoint._

import scala.annotation.implicitNotFound
/**
  * Created by soronpo on 12/26/16.
  */
import singleton.ops._
object GeneralTest {
  val d = DFUInt(2)
  d + d// Addable.DFUIntDFUInt[d.Width, (d)
  val one = 100
  d + one
//  d + (d + d).wc
  val aaa = TokenUInt(8, 3)

  val ff = DFUInt[8]
  ff.init(1, 7L, aaa, 100, Φ, Seq(aaa, aaa))

  ff.prev(1) + 5
}
