package StagesSpec

import DFiant.*
import DFiant.compiler.ir.{ClkCfg, RstCfg}
import DFiant.compiler.stages.dropRegsWires
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropRegsWiresSpec extends StageSpec:
  test("Basic wires and reg") {
    class ID extends RTDesign:
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
      """|class ID extends EDDesign:
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val r1 = DFSInt(16) <> VAR init sd"16'0"
         |  val r1_din = DFSInt(16) <> VAR
         |  process.all {
         |    val w1 = DFSInt(16) <> VAR
         |    val w2 = DFSInt(16) <> VAR
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
    class ID extends RTDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val r = DFSInt(16) <> REG
      r.din := 1
      r.din := x + r.din
      y     := r

    class IDTop extends RTDesign:
      val x    = DFSInt(16) <> IN
      val y    = DFSInt(16) <> OUT
      val id_x = DFSInt(16) <> WIRE
      val id_y = DFSInt(16) <> WIRE
      val id = new ID:
        this.x <> id_x
        this.y <> id_y
      id_x := x
      id_x := id_x + 1
      y    := id_y
    end IDTop
    val top = (new IDTop).dropRegsWires
    assertCodeString(
      top,
      """|class ID extends EDDesign:
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val r = DFSInt(16) <> VAR
         |  val r_din = DFSInt(16) <> VAR
         |  process.all {
         |    val r_din_v = DFSInt(16) <> VAR
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
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val id_x = DFSInt(16) <> VAR
         |  val id_y = DFSInt(16) <> VAR
         |  val id = new ID:
         |    this.x <>/*<--*/ id_x
         |    this.y <>/*-->*/ id_y
         |  process.all {
         |    val id_x_v = DFSInt(16) <> VAR
         |    id_x_v := x
         |    id_x_v := id_x_v + sd"2'1"
         |    y :== id_y
         |    id_x :== id_x_v
         |  }
         |end IDTop
         |""".stripMargin
    )
  }
  test("Rising clk, Async Reset") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.High)
    class ID extends RTDesign(clkCfg, rstCfg):
      val x  = DFSInt(16) <> IN
      val r1 = DFSInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val r1 = DFSInt(16) <> VAR init sd"16'0"
         |  val r1_din = DFSInt(16) <> VAR
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
    class ID extends RTDesign(clkCfg, rstCfg):
      val x  = DFSInt(16) <> IN
      val r1 = DFSInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val r1 = DFSInt(16) <> VAR init sd"16'0"
         |  val r1_din = DFSInt(16) <> VAR
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
    class ID extends RTDesign(clkCfg, rstCfg):
      val x  = DFSInt(16) <> IN
      val r1 = DFSInt(16) <> REG init 0
      r1.din := x
    val id = (new ID).dropRegsWires
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val r1 = DFSInt(16) <> VAR init sd"16'0"
         |  val r1_din = DFSInt(16) <> VAR
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
