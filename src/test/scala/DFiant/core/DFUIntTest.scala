package DFiant.core

import DFiant._
import DFiant.TestUtils._
import DFiant.tokens._
import org.scalacheck._
import scodec.bits._
import shapeless.test.illTyped

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

  property("DFUInt conversion from number") = {
    val d = 15.toDFUInt
    implicitly[d.type <:< DFUInt[4]]
    val d2 = 16.toDFUInt
    implicitly[d2.type <:< DFUInt[5]]
    val dL = 0L.toDFUInt
    implicitly[dL.type <:< DFUInt[1]]
    val dus = us(15).toDFUInt
    implicitly[dus.type <:< DFUInt[Int]]
    val dusL = us(16L).toDFUInt
    val dusBig = BigInt(31).toDFUInt
    dus.width == 4 && dusL.width == 5 && dusBig.width == 5
  }

  property("DFUInt conversion from DFBits") = {
    val a = DFBits(8).init(bin"11",20,Φ,15L)
    val b = a.toDFUInt
    implicitly[b.type <:< DFUInt[8]]
    val bi = b.getInit
    bi(0).valueUInt == BigInt(3) && bi(1).valueUInt == BigInt(20) && bi(2).isBubble && bi(3).valueUInt == BigInt(15)
  }

  property("DFUInt conversion error") = wellTyped {
    illTyped { """(-1).toDFUInt""" }
    illTyped { """(-1L).toDFUInt""" }
    illRun(us(-1).toDFUInt)
    illRun(us(-1L).toDFUInt)
    illRun(BigInt(-1).toDFUInt)
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
    (u9 + u8).wc + u8
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
    val u9 : 511 = 511
    val u8us = DFUInt(us(8))
    val u9us = us(511)
    illTyped { """u8 + u9""" }
    illRun(u8 + u9us)
    illRun(u8us + u9)
    illRun(u8us + u9us)
    illTyped { """u8 + -512""" }
    illRun(u8us + -511)
    illRun(u8us + us(-511))
  }

  property("Int + DFUInt compilable") = {
    val u8 = DFUInt(8)
    val u9 : 511 = 511
    val u8us = DFUInt(us(8))
    val u9us = us(511)
    val u98 = u9 + u8
    implicitly[u98.type <:< DFUInt[9]]
    implicitly[u98.wc.type <:< DFUInt[10]]
    implicitly[u98.c.type <:< DFBool]
    val u89 = u8.extendable + u9
    implicitly[u89.type <:< DFUInt[9]]
    implicitly[u89.wc.type <:< DFUInt[10]]
    val u98us = u9us + u8
    implicitly[u98us.type <:< DFUInt[Int]]
    implicitly[u98us.wc.type <:< DFUInt[Int]]
    implicitly[u98us.c.type <:< DFBool]
    u8.extendable + u9us
    u9 + u8us
    u8us + u8us
    u8us.extendable + u9
    u9us + u8us
    u8us.extendable + u9us
    (u9 + u8).wc + u8
    u98us.width == 9 && u98us.wc.width == 10
  }

  property("Int + DFUInt uncompilable") = wellTyped {
    val u8 : 200 = 200
    val u9 = DFUInt(9)
    val u8us = us(200)
    val u9us = DFUInt(us(9))
    illTyped { """u8 + u9""" }
    illTyped { """-511 + u9""" }
    illRun(u8 + u9us)
    illRun(u8us + u9)
    illRun(u8us + u9us)
  }
}

