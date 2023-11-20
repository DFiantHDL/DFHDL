package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.toED
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
//TODO: rethink blocking assignment in process(all) for VHDL vs. Verilog
//TODO: rethink rising_edge for VHDL vs. Verilog
class ToEDSpec extends StageSpec:
  test("Basic wires and reg") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val w1 = SInt(16) <> VAR
      val w2 = SInt(16) <> VAR
      val r1 = SInt(16) <> VAR
      w1 := x
      w1 := w1 + 1
      w2 := x
      r1 := w2.reg(1, init = 0)
      y  := w1 + r1
    val id = (new ID).toED
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val w1 = SInt(16) <> VAR
         |  val w2 = SInt(16) <> VAR
         |  val r1 = SInt(16) <> VAR
         |  process(all):
         |    w1 := x
         |    w1 := w1 + sd"16'1"
         |    w2 := x
         |    y := w1 + r1
         |  process(clk):
         |    if (clk.rising)
         |      if (rst == 1) r1 :== sd"16'0"
         |      else r1 :== w2
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("Rising clk, Async Reset") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val r1 = SInt(16) <> VAR
      r1 := x.reg(1, init = 0)
    val id = (new ID).toED
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk, rst):
         |    if (rst == 1) r1 :== sd"16'0"
         |    else if (clk.rising) r1 :== x
         |end ID
         |""".stripMargin
    )
  }
  test("Falling clk, no Reset") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Falling)
    val rstCfg = None
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val r1 = SInt(16) <> VAR
      r1 := x.reg(1, init = 0)
    val id = (new ID).toED
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk):
         |    if (clk.falling) r1 :== x
         |end ID
         |""".stripMargin
    )
  }
  test("Rising clk, Sync Reset & Active-low") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val r1 = SInt(16) <> VAR
      r1 := x.reg(1, init = 0)
    val id = (new ID).toED
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk, rst):
         |    if (rst == 0) r1 :== sd"16'0"
         |    else if (clk.rising) r1 :== x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic Hierarchy") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val r = SInt(16) <> VAR
      r := 1
      r := x + r
      y := r.reg(1, init = ?)

    class IDTop extends RTDesign(cfg):
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val temp = SInt(16) <> VAR
      val id   = new ID
      temp := x
      if (x > 0)
        temp := temp + 1
      id.x   <> temp
      y      := id.y
    end IDTop
    val top = (new IDTop).toED
    assertCodeString(
      top,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val r = SInt(16) <> VAR
         |  val r_ver_reg = SInt(16) <> VAR
         |  process(all):
         |    r := sd"16'1"
         |    r := x + r
         |    y := r_ver_reg
         |  process(clk):
         |    if (clk.rising) r_ver_reg :== r
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val temp = SInt(16) <> VAR
         |  val id = ID()
         |  id.x <> temp
         |  process(all):
         |    temp := x
         |    if (x > sd"16'0") temp := temp + sd"16'1"
         |    y := id.y
         |end IDTop
         |""".stripMargin
    )
  }
end ToEDSpec