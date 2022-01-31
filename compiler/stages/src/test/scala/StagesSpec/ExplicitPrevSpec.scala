package StagesSpec

import DFiant.*
import DFiant.compiler.stages.explicitPrev
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitPrevSpec extends StageSpec:
  test("Basic explicit prev") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT init 0
      y := y + 1
      val y2 = DFSInt(16) <> OUT init 0
      y := 1
      y := y + 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT init sd"16'0"
         |  y := y.prev
         |  y := y + sd"2'1"
         |  val y2 = DFSInt(16) <> OUT init sd"16'0"
         |  y := sd"16'1"
         |  y := y + sd"2'1"
         |end ID
         |""".stripMargin
    )
  }
  test("If-else coverage") {
    class ID(using DFC) extends DFDesign:
      val x  = DFSInt(16) <> IN
      val y  = DFSInt(16) <> OUT
      val y2 = DFSInt(16) <> OUT
      if (x > 0)
        y := 1
      y   := y + 1
      if (x > 0)
        y2 := 1
      else
        y2 := 2
      y2   := y2 + 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := y.prev
         |  val y2 = DFSInt(16) <> OUT
         |  if (x > d"16'0") y := sd"16'1"
         |  y := y + sd"2'1"
         |  if (x > d"16'0") y2 := sd"16'1"
         |  else y2 := sd"16'2"
         |  y2 := y2 + sd"2'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Partial assignment coverage") {
    class ID(using DFC) extends DFDesign:
      val x  = DFSInt(16) <> IN
      val y  = DFBits(16) <> OUT
      val y2 = DFBits(16) <> OUT
      y(7, 0)   := all(0)
      y         := y << 1
      y2(7, 0)  := all(0)
      y2(15, 8) := all(0)
      y2        := y2 << 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFBits(16) <> OUT
         |  y := y.prev
         |  val y2 = DFBits(16) <> OUT
         |  y(7, 0) := h"8'00"
         |  y := y << 1
         |  y2(7, 0) := h"8'00"
         |  y2(15, 8) := h"8'00"
         |  y2 := y2 << 1
         |end ID
         |""".stripMargin
    )
  }
end ExplicitPrevSpec
