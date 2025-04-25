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
  test("if reset else clock") {
    class ID extends EDDesign:
      val clk = Bit      <> IN
      val rst = Bit      <> IN
      val x1  = SInt(16) <> IN
      val y1  = SInt(16) <> OUT
      val x2  = SInt(16) <> IN
      val y2  = SInt(16) <> OUT
      val x3  = SInt(16) <> IN
      val y3  = SInt(16) <> OUT
      val x4  = SInt(16) <> IN
      val y4  = SInt(16) <> OUT
      val proc1 = process(clk, rst):
        if (rst == 0)
          y1 := 0
        else if (clk.rising)
          y1 := x1
      val proc2 = process(clk, rst):
        if (rst == 1)
          y2 := 0
        else if (clk.falling)
          y2 := x2
      val proc3 = process(clk, rst):
        if (rst)
          y3 := 0
        else if (clk.falling)
          y3 := x3
      val proc4 = process(clk, rst):
        if (!rst)
          y4 := 0
        else if (clk.falling)
          y4 := x4
    end ID
    val id = (new ID).vhdlProcToVerilog
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val x3 = SInt(16) <> IN
         |  val y3 = SInt(16) <> OUT
         |  val x4 = SInt(16) <> IN
         |  val y4 = SInt(16) <> OUT
         |  val proc1 = process(clk.rising, rst.falling):
         |    if (rst == 0) y1 := sd"16'0"
         |    else y1 := x1
         |  val proc2 = process(clk.falling, rst.rising):
         |    if (rst == 1) y2 := sd"16'0"
         |    else y2 := x2
         |  val proc3 = process(clk.falling, rst.rising):
         |    if (rst) y3 := sd"16'0"
         |    else y3 := x3
         |  val proc4 = process(clk.falling, rst.falling):
         |    if (!rst) y4 := sd"16'0"
         |    else y4 := x4
         |end ID
         |""".stripMargin
    )
  }
  test("internal clk/rst ports") {
    class ClkGen extends EDDesign:
      val clk = Bit <> OUT
      clk <> 1
    class ClkRstGen extends EDDesign:
      val clk = Bit <> OUT
      val rst = Bit <> OUT
      clk <> 1
      rst <> 0

    class Foo extends EDDesign:
      val y         = Bit <> IN
      val z         = Bit <> OUT
      val z2        = Bit <> OUT
      val z3        = Bit <> OUT
      val clkGen    = ClkGen()
      val clkRstGen = ClkRstGen()
      process(clkGen.clk):
        if (clkGen.clk.rising) println(y)
      process(clkRstGen.clk, clkRstGen.rst):
        if (clkRstGen.rst)
          z := 0
        else if (clkRstGen.clk.rising)
          z := 1
      process(clkRstGen.clk, clkRstGen.rst):
        if (clkRstGen.rst == 0)
          z2 := 0
        else if (clkRstGen.clk.falling)
          z2 := 1
      process(clkRstGen.clk, clkRstGen.rst):
        if (!clkRstGen.rst)
          z3 := 0
        else if (clkRstGen.clk.falling)
          z3 := 1
    end Foo
    val top = (new Foo).vhdlProcToVerilog
    assertCodeString(
      top,
      """|class ClkGen extends EDDesign:
         |  val clk = Bit <> OUT
         |  clk <> 1
         |end ClkGen
         |
         |class ClkRstGen extends EDDesign:
         |  val clk = Bit <> OUT
         |  val rst = Bit <> OUT
         |  clk <> 1
         |  rst <> 0
         |end ClkRstGen
         |
         |class Foo extends EDDesign:
         |  val y = Bit <> IN
         |  val z = Bit <> OUT
         |  val z2 = Bit <> OUT
         |  val z3 = Bit <> OUT
         |  val clkGen = ClkGen()
         |  val clkRstGen = ClkRstGen()
         |  process(clkGen.clk.rising):
         |    println(s"${y}")
         |  process(clkRstGen.clk.rising, clkRstGen.rst.rising):
         |    if (clkRstGen.rst) z := 0
         |    else z := 1
         |  process(clkRstGen.clk.falling, clkRstGen.rst.falling):
         |    if (clkRstGen.rst == 0) z2 := 0
         |    else z2 := 1
         |  process(clkRstGen.clk.falling, clkRstGen.rst.falling):
         |    if (!clkRstGen.rst) z3 := 0
         |    else z3 := 1
         |end Foo
         |""".stripMargin
    )
  }
end VHDLProcToVerilogSpec
