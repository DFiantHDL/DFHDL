package StagesSpec

import DFiant.*
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
      """|class ID(using DFC) extends RTDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val r1 = DFSInt(16) <> VAR init 0
         |  val r1_din_sig = DFSInt(16) <> VAR
         |  always.all {
         |    val w1 = DFSInt(16) <> VAR
         |    val w2 = DFSInt(16) <> VAR
         |    val r1_din = DFSInt(16) <> VAR
         |    w1 := x
         |    w1 := w1 + 1
         |    w2 := x
         |    r1_din := w2
         |    y := w1 + r1
         |    r1_din_sig := r1_din
         |  }
         |  always(clk, rst) {
         |    if (rst)
         |      r1 := 0
         |    else if (clk.rising)
         |      r1 := r1_din_sig
         |  }
         |end ID
         |""".stripMargin
    )
  }
end DropRegsWiresSpec
