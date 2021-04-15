package DFiant

import DFiant.TestUtils._

class DFUIntBitsConvSpec extends DFTopSpec {
  @df class Top extends DFDesign {
    val xu = DFUInt(8) <> IN
    val yb = DFBits(8) <> OUT
    val xb = DFBits(8) <> IN
    val yu = DFUInt(8) <> OUT
    yb := xu
    yb := d"8'25"
    yu := xb
    yu := h"19"
  }

  val top = new Top

  val expectedCodeString : String =
    """|@df final class Top extends DFDesign {
       |  val xu = DFUInt(8) <> IN
       |  val yb = DFBits(8) <> OUT
       |  val xb = DFBits(8) <> IN
       |  val yu = DFUInt(8) <> OUT
       |  yb     := xu.bits
       |  yb     := h"8'19"
       |  yu     := xb.uint
       |  yu     := 25
       |}""".stripMargin

  test("codeString generation") {
    assert(top.codeString =@= expectedCodeString)
  }
}

