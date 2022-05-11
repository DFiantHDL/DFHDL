package StagesSpec

import DFiant.*
import DFiant.compiler.stages.dropRegsWires
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropRegsWiresSpec extends StageSpec:
  test("Basic wires and reg") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val w1 = SInt(16) <> WIRE
      val w2 = SInt(16) <> WIRE
      val r1 = SInt(16) <> REG init 0
      w1     := x
      w1     := w1 + 1
      w2     := x
      r1.din := w2
      y      := w1 + r1
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val r1 = SInt(16) <> VAR init sd"16'0"
         |  val r1_din = SInt(16) <> VAR
         |  process.all {
         |    val w1 = SInt(16) <> VAR
         |    val w2 = SInt(16) <> VAR
         |    w1 := x
         |    w1 := w1 + sd"2'1"
         |    w2 := x
         |    r1_din :== w2
         |    y :== w1 + r1
         |  }
         |  process(clk) {
         |    if (clk.rising)
         |      if (rst == 1) r1 :== sd"16'0"
         |      else r1 :== r1_din
         |  }
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
      val r = SInt(16) <> REG
      r.din := 1
      r.din := x + r.din
      y     := r

    class IDTop extends RTDesign(cfg):
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val temp = SInt(16) <> WIRE
      val id   = new ID
      temp := x
      temp := temp + 1
      id.x <> temp
      y    := id.y
    end IDTop
    val top = (new IDTop).dropRegsWires
    assertCodeString(
      top,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val r = SInt(16) <> VAR
         |  val r_din = SInt(16) <> VAR
         |  process.all {
         |    val r_din_v = SInt(16) <> VAR
         |    r_din_v := sd"16'1"
         |    r_din_v := x + r_din_v
         |    y :== r
         |    r_din :== r_din_v
         |  }
         |  process(clk) {
         |    if (clk.rising) r :== r_din
         |  }
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val temp = SInt(16) <> VAR
         |  val id_clk = Bit <> VAR
         |  val id_rst = Bit <> VAR
         |  val id_x = SInt(16) <> VAR
         |  val id_y = SInt(16) <> VAR
         |  val id = new ID:
         |    this.clk <>/*<--*/ id_clk
         |    this.rst <>/*<--*/ id_rst
         |    this.x <>/*<--*/ id_x
         |    this.y <>/*-->*/ id_y
         |  process.all {
         |    val temp_v = SInt(16) <> VAR
         |    temp_v := x
         |    temp_v := temp_v + sd"2'1"
         |    id_x <> temp_v
         |    y :== id_y
         |    temp :== temp_v
         |  }
         |end IDTop
         |""".stripMargin
    )
  }
  test("Rising clk, Async Reset") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x  = SInt(16) <> IN
      val r1 = SInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR init sd"16'0"
         |  val r1_din = SInt(16) <> VAR
         |  process.all {
         |    r1_din :== x
         |  }
         |  process(clk, rst) {
         |    if (rst == 1) r1 :== sd"16'0"
         |    else if (clk.rising) r1 :== r1_din
         |  }
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
      val r1 = SInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR init sd"16'0"
         |  val r1_din = SInt(16) <> VAR
         |  process.all {
         |    r1_din :== x
         |  }
         |  process(clk) {
         |    if (clk.falling) r1 :== r1_din
         |  }
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
      val r1 = SInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR init sd"16'0"
         |  val r1_din = SInt(16) <> VAR
         |  process.all {
         |    r1_din :== x
         |  }
         |  process(clk, rst) {
         |    if (rst == 0) r1 :== sd"16'0"
         |    else if (clk.rising) r1 :== r1_din
         |  }
         |end ID
         |""".stripMargin
    )
  }
end DropRegsWiresSpec
