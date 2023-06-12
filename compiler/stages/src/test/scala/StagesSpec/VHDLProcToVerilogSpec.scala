package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.vhdlProcToVerilog
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class VHDLProcToVerilogSpec extends StageSpec:
  test("Only clock") {
    class ID extends EDDesign:
      val clk = Bit      <> IN
      val x1  = SInt(16) <> IN
      val y1  = SInt(16) <> OUT
      val x2  = SInt(16) <> IN
      val y2  = SInt(16) <> OUT
      val proc1 = process(clk):
        if (clk.rising)
          y1 := x1
      val proc2 = process(clk):
        if (clk.falling)
          y2 := x2
    val id = (new ID).vhdlProcToVerilog
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val proc1 = process(clk.rising):
         |    y1 := x1
         |  val proc2 = process(clk.falling):
         |    y2 := x2
         |end ID
         |""".stripMargin
    )
  }
end VHDLProcToVerilogSpec
