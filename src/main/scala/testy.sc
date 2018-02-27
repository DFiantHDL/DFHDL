import DFiant._
import shapeless.test.illTyped
def nf(t : Int) = t
def nf(t : Long) = t
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

import GlobalDesign._
val u8 = DFUInt(8)
val u8nf = DFUInt(nf(8))
type U8 = u8nf.TVal
new DFDesign {
  val u8p01 : U8 <> IN = u8
  val u8p02 : U8 <> OUT = u8
  val u8p03 : U8 <> IN = OPEN
  val u8p04 : U8 <> OUT = OPEN
  val u8p05 : U8 <> IN = 1
  val u8p06 : U8 <> IN = 1L
  val u8p07 : U8 <> IN = nf(1)
  val u8p08 : U8 <> IN = nf(1L)
  val u8p09 : U8 <> IN = BigInt(1)
  val u8p10 : U8 <> IN = u8 + u8
  val u8p11 : U8 <> IN = u8nf
  val u8p12 : U8 <> OUT = u8nf
  illTyped { """val u8p28 : U8 <> OUT = u8 + u8""" }
  illTyped { """val u8p29 : U8 <> OUT = 1""" }
//  illRun { val u8p30 : U8 <> IN = 500 }
//  illRun { val u8p31 : U8 <> IN = 500L }
  illTyped { """val u8p32 : U8 <> IN = -1""" }
  illTyped { """val u8p33 : U8 <> IN = -1L""" }
  illRun {val u8p34 : U8 <> IN = nf(500)}
  illRun {val u8p35 : U8 <> IN = nf(500L)}
  illRun {val u8p36 : U8 <> IN = nf(-1)}
  illRun {val u8p37 : U8 <> IN = nf(-1L)}
  illRun {val u8p38 : U8 <> IN = BigInt(500)}
  illRun {val u8p39 : U8 <> IN = BigInt(-1)}

  u8p01 + u8p02
  u8p07 + u8p05
}
