package DFiant
import org.scalacheck._
import shapeless.test.illTyped
import singleton.twoface._

object TestUtils {
  def illRun(body: => Unit) : Boolean = {
    val isIll = try {
      body
      false
    } catch {
      case _ : Throwable =>
      true
    }
    if (!isIll)
      assert(false, "Expected assertion did not occur")
    true
  }

  type WorkAround0[T]
  object WorkAround0 extends _root_.singleton.twoface.impl.Checked0Param.Builder[Nothing, WorkAround0, WorkAround0, Nothing]

  type WorkAround1[T,P]
  object WorkAround1 extends _root_.singleton.twoface.impl.Checked1Param.Builder[Nothing, WorkAround1, WorkAround1, Nothing, Nothing]

}
import TestUtils._

class SafeBitsSelectionFromSafeVarSpec extends Properties("SafeBitsSelectionFromSafeVarSpec") {
  property("DFBits[W] @ W < 0 compile error") = {
    illTyped { """DFBits[0]""" }
    illTyped { """DFBits[-1]""" }
    true
  }
  property("DFBits[4].width") = {
    val a = DFBits[4]
    implicitly[a.Width =:= 4]
    a.width.getValue == 4
  }
  property("DFBits[4].bits(2,1)") = {
    val a = DFBits[4]; val b = a.bits(2,1)
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].apply(2,1)") = {
    val a = DFBits[4]; val b = a(2,1)
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].bits[2,1]") = {
    val a = DFBits[4]; val b = a.bits[2,1]
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].apply[2,1]") = {
    val a = DFBits[4]; val b = a[2,1]
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].bits(1,1)") = {
    val a = DFBits[4]; val b = a.bits(1,1)
    implicitly[b.Width =:= 1]
    b.width.getValue == 1
  }
  property("DFBits[4].apply[1,1]") = {
    val a = DFBits[4]; val b = a[1,1]
    implicitly[b.Width =:= 1]
    b.width.getValue == 1
  }
  property("DFBits[4].msbits(2)") = {
    val a = DFBits[4]; val b = a.msbits(2)
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].msbits[4]") = {
    val a = DFBits[4]; val b = a.msbits[4]
    implicitly[b.Width =:= 4]
    b.width.getValue == 4
  }
  property("DFBits[4].lsbits(2)") = {
    val a = DFBits[4]; val b = a.lsbits(2)
    implicitly[b.Width =:= 2]
    b.width.getValue == 2
  }
  property("DFBits[4].lsbits[4]") = {
    val a = DFBits[4]; val b = a.lsbits[4]
    implicitly[b.Width =:= 4]
    b.width.getValue == 4
  }
  property("DFBits[4] out of range bits range selections compile error") = {
    var three = 3
    illTyped { """val a = DFBits[4]; a.bits(4,three)""" }
    illTyped { """val a = DFBits[4]; a.bits(4,3)""" }
    illTyped { """val a = DFBits[4]; a.bits(-1,-2)""" }
    illTyped { """val a = DFBits[4]; a.bits(1,-2)""" }
    illTyped { """val a = DFBits[4]; a.bits(1,3)""" }
    illTyped { """val a = DFBits[4]; a.bits[4,3]""" }
    illTyped { """val a = DFBits[4]; a.bits[-1,-2]""" }
    illTyped { """val a = DFBits[4]; a.bits[1,-2]""" }
    illTyped { """val a = DFBits[4]; a.bits[1,3]""" }
    illTyped { """val a = DFBits[4]; a.msbits(0)""" }
    illTyped { """val a = DFBits[4]; a.msbits(-1)""" }
    illTyped { """val a = DFBits[4]; a.msbits(5)""" }
    illTyped { """val a = DFBits[4]; a.lsbits(0)""" }
    illTyped { """val a = DFBits[4]; a.lsbits(-1)""" }
    illTyped { """val a = DFBits[4]; a.lsbits(5)""" }
    illTyped { """val a = DFBits[4]; a(4,3)""" }
    illTyped { """val a = DFBits[4]; a(-1,-2)""" }
    illTyped { """val a = DFBits[4]; a(1,-2)""" }
    illTyped { """val a = DFBits[4]; a(1,3)""" }
    illTyped { """val a = DFBits[4]; a[4,3]""" }
    illTyped { """val a = DFBits[4]; a[-1,-2]""" }
    illTyped { """val a = DFBits[4]; a[1,-2]""" }
    illTyped { """val a = DFBits[4]; a[1,3]""" }
    true
  }
  property("DFBits[4].bit(1)") = {
    val a = DFBits[4]; val b : DFBool = a.bit(1)
    b.width.getValue == 1
  }
  property("DFBits[4].apply(1)") = {
    val a = DFBits[4]; val b : DFBool = a(1)
    b.width.getValue == 1
  }
  property("DFBits[4].bit[1]") = {
    val a = DFBits[4]; val b : DFBool = a.bit[1]
    b.width.getValue == 1
  }
  property("DFBits[4].apply[1]") = {
    val a = DFBits[4]; val b : DFBool = a[1]
    b.width.getValue == 1
  }
  property("DFBits[4] out of range single bit selections compile error") = {
    illTyped { """val a = DFBits[4]; a.bit(4)""" }
    illTyped { """val a = DFBits[4]; a.bit(-1)""" }
    illTyped { """val a = DFBits[4]; a.bit[4]""" }
    illTyped { """val a = DFBits[4]; a.bit[-1]""" }
    illTyped { """val a = DFBits[4]; a(4)""" }
    illTyped { """val a = DFBits[4]; a(-1)""" }
    true
  }
}


class UnsafeBitsSelectionFromSafeVarSpec extends Properties("UnsafeBitsSelectionFromSafeVarSpec") {
  var two = 2
  var one = 1
  var neg_two = -2
  var neg_one = -1
  var three = 3
  var four = 4
  var five = 5
  property("DFBits[4].bits(two,1)") = {
    val a = DFBits[4]; val b = a.bits(two,1)
    implicitly[b.Width =:= Int]
    b.width.getValue == 2
  }
  property("DFBits[4].bits(2,one)") = {
    val a = DFBits[4]; val b = a.bits(2,one)
    implicitly[b.Width =:= Int]
    b.width.getValue == 2
  }
  property("DFBits[4].bits(two,one)") = {
    val a = DFBits[4]; val b = a.bits(two,one)
    implicitly[b.Width =:= Int]
    b.width.getValue == 2
  }
  property("DFBits[4].bits(1,one)") = {
    val a = DFBits[4]; val b = a.bits(1,one)
    implicitly[b.Width =:= Int]
    b.width.getValue == 1
  }
  property("DFBits[4].bits(one,1)") = {
    val a = DFBits[4]; val b = a.bits(one,1)
    implicitly[b.Width =:= Int]
    b.width.getValue == 1
  }
  property("DFBits[4].bits(one,one)") = {
    val a = DFBits[4]; val b = a.bits(one,one)
    implicitly[b.Width =:= Int]
    b.width.getValue == 1
  }
  property("DFBits[4].msbits(two)") = {
    val ttwo = 2
    val a = DFBits[4]; val b = a.msbits(ttwo)
    implicitly[b.Width =:= Int]
    b.width.getValue == 2
  }
  property("DFBits[4].lsbits(two)") = {
    val a = DFBits[4]; val b = a.lsbits(two)
    implicitly[b.Width =:= Int]
    b.width.getValue == 2
  }
  property("DFBits[4] out of range bits range selections exception") = {
    illRun { val a = DFBits[4]; a.bits(neg_one,neg_two) }
    illRun { val a = DFBits[4]; a.bits(one,neg_two) }
    illRun { val a = DFBits[4]; a.bits(one,three) }
    illRun { val a = DFBits[4]; a.msbits(neg_one) }
    illRun { val a = DFBits[4]; a.lsbits(five) }
    true
  }
  property("DFBits[4].bit(1)") = {
    val a = DFBits[4]; val b = a.bit(1)
    implicitly[b.Width =:= 1]
    b.width.getValue == 1
  }
  property("DFBits[4].bit[1]") = {
    val a = DFBits[4]; val b = a.bit[1]
    implicitly[b.Width =:= 1]
    b.width.getValue == 1
  }
  property("DFBits[4] out of range single bit selections compile error") = {
    illRun { val a = DFBits[4]; a.bit(four) }
    illRun { val a = DFBits[4]; a.bit(neg_one) }
  }
}