package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import singleton.twoface._
import TestUtils._

class DFUIntTest extends Properties("DFUIntTestSpec") {
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

