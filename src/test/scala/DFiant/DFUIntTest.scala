package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import singleton.twoface._
import TestUtils._

class DFUIntTest extends Properties("DFUIntTestSpec") {
  property("DFUInt[W] @ W < 0 compile error") = wellTyped {
    illTyped { """DFUInt[0]""" }
    illTyped { """DFUInt[-1]""" }
    illTyped { """DFUInt(0)""" }
    illTyped { """DFUInt(-1)""" }
    illRun(DFUInt(us(0)))
  }
  property("DFUInt construction") = {
    val a = DFUInt(1)
    implicitly[a.type <:< DFUInt[1]]
    val b = DFUInt[2]
    implicitly[b.type <:< DFUInt[2]]
    val c = DFUInt(us(8))
    implicitly[c.type <:< DFUInt[Int]]
    c.width == 8
  }

  property("DFUInt + DFUInt compilable") = {
    val u8 = DFUInt(8)
    val u9 = DFUInt(9)
    val u8us = DFUInt(us(8))
    val u9us = DFUInt(us(9))
    val u98 = u9 + u8
    implicitly[u98.type <:< DFUInt[9]]
    implicitly[u98.wc.type <:< DFUInt[10]]
    implicitly[u98.c.type <:< DFBool]
    val u88 = u8 + u8
    implicitly[u88.type <:< DFUInt[8]]
    val u89 = u8.extendable + u9
    implicitly[u89.type <:< DFUInt[9]]
    implicitly[u89.wc.type <:< DFUInt[10]]
    val u98us = u9us + u8
    implicitly[u98us.type <:< DFUInt[Int]]
    implicitly[u98us.wc.type <:< DFUInt[Int]]
    implicitly[u98us.c.type <:< DFBool]
    u8 + u8
    u8.extendable + u9us
    u9 + u8us
    u8us + u8us
    u8us.extendable + u9
    u9us + u8us
    u8us + u8us
    u8us.extendable + u9us
    u98us.width == 9 && u98us.wc.width == 10
  }

  property("DFUInt + DFUInt uncompilable") = wellTyped {
    val u8 = DFUInt(8)
    val u9 = DFUInt(9)
    val u8us = DFUInt(us(8))
    val u9us = DFUInt(us(9))
    illTyped { """u8 + u9""" }
    illRun(u8 + u9us)
    illRun(u8us + u9)
    illRun(u8us + u9us)
  }

  property("DFUInt + Int compilable") = {
    val u8 : 200 = 200
    val u9 = DFUInt(9)
    val u8us = us(200)
    val u9us = DFUInt(us(9))
    val u98 = u9 + u8
    implicitly[u98.type <:< DFUInt[9]]
    implicitly[u98.wc.type <:< DFUInt[10]]
    implicitly[u98.c.type <:< DFBool]
    val u98us = u9us + u8
    implicitly[u98us.type <:< DFUInt[Int]]
    implicitly[u98us.wc.type <:< DFUInt[Int]]
    implicitly[u98us.c.type <:< DFBool]
    u9 + u8us
    u9us + u8us
    u9 + -1
    u9 + us(-1)
    u9us + -1
    u9us + us(-1)
    u98us.width == 9 && u98us.wc.width == 10
  }

  property("DFUInt + Int uncompilable") = wellTyped {
    val u8 = DFUInt(8)
    val u9 : 512 = 512
    val u8us = DFUInt(us(8))
    val u9us = us(512)
    illTyped { """u8 + u9""" }
    illRun(u8 + u9us)
    illRun(u8us + u9)
    illRun(u8us + u9us)
    illTyped { """u8 + -512""" }
    illRun(u8us + -512)
    illRun(u8us + us(-512))
  }
}

