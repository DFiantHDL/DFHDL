package StagesSpec

import DFiant.*
import DFiant.compiler.ir.DomainType.RT.ClockParams
import DFiant.compiler.stages.dropRegsWires
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropRegsWiresSpec extends StageSpec:
  test("Drop wires") {
    class ID(using DFC) extends RTDesign:
      val x  = DFSInt(16) <> IN
      val y  = DFSInt(16) <> OUT
      val w1 = DFSInt(16) <> WIRE
      val w2 = DFSInt(16) <> WIRE
      val r1 = DFSInt(16) <> REG init 0
      w1     := x
      w1     := w1 + 1
      w2     := x
      r1.din := w2
      y      := w1 + r1
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID(using DFC) extends LLRTDesign:
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val r1 = DFSInt(16) <> VAR init sd"16'0"
         |  val r1_din = DFSInt(16) <> VAR
         |  always.all {
         |    val w1 = DFSInt(16) <> VAR
         |    val w2 = DFSInt(16) <> VAR
         |    w1 := x
         |    w1 := w1 + sd"2'1"
         |    w2 := x
         |    r1_din := w2
         |    y := w1 + r1
         |  }
         |  always(clk, rst) {
         |    if (rst.bool)
         |      r1 := 0
         |    else if (clk.rising)
         |      r1 := r1_din
         |  }
         |end ID
         |""".stripMargin
    )
  }
end DropRegsWiresSpec
