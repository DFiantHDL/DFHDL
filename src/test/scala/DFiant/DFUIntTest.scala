//package DFiant
//
//import DFiant.TestUtils._
//import org.scalacheck._
//import scodec.bits._
//import shapeless.test.illTyped
//
//class DFUIntTest extends Properties("DFUIntTestSpec") {
//  import GlobalDesign._
//  property("DFUInt[W] @ W < 0 compile error") = wellTyped {
//    illTyped { """DFUInt[0]""" }
//    illTyped { """DFUInt[-1]""" }
//    illTyped { """DFUInt(0)""" }
//    illTyped { """DFUInt(-1)""" }
//    illRun(DFUInt(nf(0)))
//  }
//
//  property("DFUInt construction") = {
//    val a = DFUInt(1)
//    implicitly[a.type <:< DFUInt[1]]
//    val b = DFUInt[2]
//    implicitly[b.type <:< DFUInt[2]]
//    val c = DFUInt(nf(8))
//    implicitly[c.type <:< DFUInt[Int]]
//    c.width == 8
//  }
//
//  property("DFUInt conversion from number") = {
//    val d = 15.toDFUInt
//    implicitly[d.type <:< DFUInt[4]]
//    val d2 = 16.toDFUInt
//    implicitly[d2.type <:< DFUInt[5]]
//    val dL = 0L.toDFUInt
//    implicitly[dL.type <:< DFUInt[1]]
//    val dus = nf(15).toDFUInt
//    implicitly[dus.type <:< DFUInt[Int]]
//    val dusL = nf(16L).toDFUInt
//    val dusBig = BigInt(31).toDFUInt
//    dus.width == 4 && dusL.width == 5 && dusBig.width == 5
//  }
//
//  property("DFUInt conversion from DFBits") = {
//    val a = DFBits(8).init(bin"11",20,Φ,15L)
//    val b = a.toDFUInt
//    implicitly[b.type <:< DFUInt[8]]
//    val bi = b.getInit
//    bi(0).valueUInt == BigInt(3) && bi(1).valueUInt == BigInt(20) && bi(2).isBubble && bi(3).valueUInt == BigInt(15)
//  }
//
//  property("DFUInt conversion error") = wellTyped {
//    illTyped { """(-1).toDFUInt""" }
//    illTyped { """(-1L).toDFUInt""" }
//    illRun(nf(-1).toDFUInt)
//    illRun(nf(-1L).toDFUInt)
//    illRun(BigInt(-1).toDFUInt)
//  }
//
//  property("DFUInt := compilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u7 = DFUInt(7)
//    u8 := 1
//    u8 := 1L
//    u8 := u7
//    u8 := BigInt(1)
//  }
//
//  property("DFUInt := uncompilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u7 = DFUInt(7)
//    illTyped { """u8 := 500""" }
//    illTyped { """u8 := 500L""" }
//    illTyped { """u7 := u8""" }
//    illRun {u8 := BigInt(500)}
//  }
//
//  property("DFUInt == compilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u7 = DFUInt(7)
//    u8 == 1
//    u8 == 1L
//    u8 == BigInt(1)
//    u8 == (u7 + 1).wc
//  }
//
//  property("DFUInt == uncompilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u7 = DFUInt(7)
//    illTyped { """u8 == u7""" }
//    illTyped { """u8 == -1""" }
//    illTyped { """u8 == -1L""" }
//    illRun {u8 == BigInt(-1)}
//  }
//
//  property("DFUInt + DFUInt compilable") = {
//    val u8 = DFUInt(8)
//    val u9 = DFUInt(9)
//    val u8us = DFUInt(nf(8))
//    val u9us = DFUInt(nf(9))
//    val u98 = u9 + u8
//    implicitly[u98.type <:< DFUInt[9]]
//    implicitly[u98.wc.type <:< DFUInt[10]]
//    implicitly[u98.c.type <:< DFBits[1]]
//    val u88 = u8 + u8
//    implicitly[u88.type <:< DFUInt[8]]
//    val u89 = u8.extendable + u9
//    implicitly[u89.type <:< DFUInt[9]]
//    implicitly[u89.wc.type <:< DFUInt[10]]
//    val u98us = u9us + u8
//    implicitly[u98us.type <:< DFUInt[Int]]
//    implicitly[u98us.wc.type <:< DFUInt[Int]]
//    implicitly[u98us.c.type <:< DFBits[1]]
//    u8 + u8
//    u8.extendable + u9us
//    u9 + u8us
//    u8us + u8us
//    u8us.extendable + u9
//    u9us + u8us
//    u8us + u8us
//    u8us.extendable + u9us
//    (u9 + u8).wc + u8
//    u98us.width == 9 && u98us.wc.width == 10
//  }
//
//  property("DFUInt + DFUInt uncompilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u9 = DFUInt(9)
//    val u8us = DFUInt(nf(8))
//    val u9us = DFUInt(nf(9))
//    illTyped { """u8 + u9""" }
//    illRun(u8 + u9us)
//    illRun(u8us + u9)
//    illRun(u8us + u9us)
//  }
//
//  property("DFUInt + Int compilable") = {
//    val u8 : 200 = 200
//    val u9 = DFUInt(9)
//    val u8us = nf(200)
//    val u9us = DFUInt(nf(9))
//    val u98 = u9 + u8
//    implicitly[u98.type <:< DFUInt[9]]
//    implicitly[u98.wc.type <:< DFUInt[10]]
//    implicitly[u98.c.type <:< DFBits[1]]
//    val u98us = u9us + u8
//    implicitly[u98us.type <:< DFUInt[Int]]
//    implicitly[u98us.wc.type <:< DFUInt[Int]]
//    implicitly[u98us.c.type <:< DFBits[1]]
//    u9 + u8us
//    u9us + u8us
//    u9 + -1
//    u9 + nf(-1)
//    u9us + -1
//    u9us + nf(-1)
//    u98us.width == 9 && u98us.wc.width == 10
//  }
//
//  property("DFUInt + Int uncompilable") = wellTyped {
//    val u8 = DFUInt(8)
//    val u9 : 511 = 511
//    val u8us = DFUInt(nf(8))
//    val u9us = nf(511)
//    illTyped { """u8 + u9""" }
//    illRun(u8 + u9us)
//    illRun(u8us + u9)
//    illRun(u8us + u9us)
//    illTyped { """u8 + -512""" }
//    illRun(u8us + -511)
//    illRun(u8us + nf(-511))
//  }
//
//  property("Int + DFUInt compilable") = {
//    val u8 = DFUInt(8)
//    val u9 : 511 = 511
//    val u8us = DFUInt(nf(8))
//    val u9us = nf(511)
//    val u98 = u9 + u8
//    implicitly[u98.type <:< DFUInt[9]]
//    implicitly[u98.wc.type <:< DFUInt[10]]
//    implicitly[u98.c.type <:< DFBits[1]]
//    val u89 = u8.extendable + u9
//    implicitly[u89.type <:< DFUInt[9]]
//    implicitly[u89.wc.type <:< DFUInt[10]]
//    val u98us = u9us + u8
//    implicitly[u98us.type <:< DFUInt[Int]]
//    implicitly[u98us.wc.type <:< DFUInt[Int]]
//    implicitly[u98us.c.type <:< DFBits[1]]
//    u8.extendable + u9us
//    u9 + u8us
//    u8us + u8us
//    u8us.extendable + u9
//    u9us + u8us
//    u8us.extendable + u9us
//    (u9 + u8).wc + u8
//    u98us.width == 9 && u98us.wc.width == 10
//  }
//
//  property("Int + DFUInt uncompilable") = wellTyped {
//    val u8 : 200 = 200
//    val u9 = DFUInt(9)
//    val u8us = nf(200)
//    val u9us = DFUInt(nf(9))
//    illTyped { """u8 + u9""" }
//    illTyped { """-511 + u9""" }
//    illRun(u8 + u9us)
//    illRun(u8us + u9)
//    illRun(u8us + u9us)
//  }
//
//  property("Final Port conversions") = wellTyped {
//    val u7 = DFUInt(7)
//    val u7nf = DFUInt(nf(7))
//    val u8 = DFUInt(8)
//    val u8nf = DFUInt(nf(8))
//    val u9 = DFUInt(9)
//    val u9nf = DFUInt(nf(9))
//    type U8 = u8.TVal
//    new DFDesign {
//      val u8p01: U8 <> IN = u8
//      val u8p02: U8 <> OUT = u8
//      val u8p03: U8 <> IN = OPEN
//      val u8p04: U8 <> OUT = OPEN
//      val u8p05: U8 <> IN = 1
//      val u8p06: U8 <> IN = 1L
//      val u8p07: U8 <> IN = nf(1)
//      val u8p08: U8 <> IN = nf(1L)
//      val u8p09: U8 <> IN = BigInt(1)
//      val u8p10: U8 <> IN = u8 + u8
//      val u8p11: U8 <> IN = u8nf
//      val u8p12: U8 <> OUT = u8nf
//      val u8p13: U8 <> IN = u7.extendable
//      val u8p14: U8 <> IN = u7nf.extendable
//      illTyped { """val u8p25 : U8 <> IN = u7"""}
//      illTyped { """val u8p26 : U8 <> OUT = u7.extendable"""}
//      illTyped { """val u8p27 : U8 <> IN = u9"""}
//      illTyped { """val u8p28 : U8 <> OUT = u8 + u8"""}
//      illTyped { """val u8p29 : U8 <> OUT = 1"""}
//      illTyped { """val u8p30 : U8 <> IN = 500"""}
//      illTyped { """val u8p31 : U8 <> IN = 500L"""}
//      illTyped { """val u8p32 : U8 <> IN = -1"""}
//      illTyped { """val u8p33 : U8 <> IN = -1L"""}
//      illRun {val u8p34 : U8 <> IN = nf(500)}
//      illRun {val u8p35 : U8 <> IN = nf(500L)}
//      illRun {val u8p36 : U8 <> IN = nf(-1)}
//      illRun {val u8p37 : U8 <> IN = nf(-1L)}
//      illRun {val u8p38 : U8 <> IN = BigInt(500)}
//      illRun {val u8p39 : U8 <> IN = BigInt(-1)}
//      illRun {val u8p40 : U8 <> IN = u7nf}
//      illRun {val u8p41 : U8 <> IN = u9nf}
//
//      u8p01 + u8p02
//      u8p07 + u8p05
//    }
//  }
//
//  property("Non-Final Port conversions") = wellTyped {
//    val u8 = DFUInt(8)
//    val u8nf = DFUInt(nf(8))
//    type UI = DFUInt[Int]
//    new DFDesign {
//      val u8p01 : UI <> IN = u8
//      val u8p02 : UI <> OUT = u8
//      val u8p03 : UI <> IN = OPEN
//      val u8p04 : UI <> OUT = OPEN
//      val u8p05 : UI <> IN = 1
//      val u8p06 : UI <> IN = 1L
//      val u8p07 : UI <> IN = nf(1)
//      val u8p08 : UI <> IN = nf(1L)
//      val u8p09 : UI <> IN = BigInt(1)
//      val u8p10 : UI <> IN = u8 + u8
//      val u8p11 : UI <> IN = u8nf
//      val u8p12 : UI <> OUT = u8nf
//      illTyped { """val u8p28 : UI <> OUT = u8 + u8""" }
//      illTyped { """val u8p29 : UI <> OUT = 1""" }
//      val u8p30 : UI <> IN = 500
//      val u8p31 : UI <> IN = 500L
//      illTyped { """val u8p32 : UI <> IN = -1""" }
//      illTyped { """val u8p33 : UI <> IN = -1L""" }
//      val u8p34 : UI <> IN = nf(500)
//      val u8p35 : UI <> IN = nf(500L)
//      illRun {val u8p36 : UI <> IN = nf(-1)}
//      illRun {val u8p37 : UI <> IN = nf(-1L)}
//      val u8p38 : UI <> IN = BigInt(500)
//      illRun {val u8p39 : UI <> IN = BigInt(-1)}
//
//      u8p01 + u8p02
//      u8p07 + u8p05
//    }
//  }
//}
//
