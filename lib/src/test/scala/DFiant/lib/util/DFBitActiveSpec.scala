package DFiant.lib.util

import DFiant._
import TestUtils._

@df class DFBitActiveDesign extends DFDesign {
  val i = DFBit.ActiveHigh <> IN
  val o1 = DFBit.ActiveHigh <> OUT
  val o2 = DFBit.ActiveLow <> OUT
  val o3 = DFBit.ActiveLow <> OUT
  o1 := DFBit.Status.Active
  o2 := DFBit.Status.Active
  o3 := i
}
class DFBitActiveSpec extends DFTopSpec {
  val top = new DFBitActiveDesign

  test("codeString generation") {
    val expectedCodeString : String =
      """|@df final class DFBitActiveDesign extends DFDesign {
         |  val i  = ActiveHigh <> IN
         |  val o1 = ActiveHigh <> OUT
         |  val o2 = ActiveLow  <> OUT
         |  val o3 = ActiveLow  <> OUT
         |  o1     := ActiveHigh(1)
         |  o2     := ActiveLow(0)
         |  o3     := (!i.actual).as(ActiveLow)
         |}""".stripMargin
    assert(top.codeString =@= expectedCodeString)
  }

  test("compiled codeString generation") {
    import compiler.backend.vhdl.v2008
    val expectedCodeString : String =
      """|@df final class DFBitActiveDesign extends DFDesign {
         |  val i  = DFBit <> IN
         |  val o1 = DFBit <> OUT
         |  val o2 = DFBit <> OUT
         |  val o3 = DFBit <> OUT
         |  o1     := 1
         |  o2     := 0
         |  o3     := !i
         |}""".stripMargin
    assert(top.compile.codeString =@= expectedCodeString)
  }
}