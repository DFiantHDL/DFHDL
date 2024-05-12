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
  test("Basic Bits Counter") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class Counter(val width: Int <> CONST = 8) extends RTDesign(cfg):
      val cnt = Bits(width) <> OUT init all(0)
      cnt := cnt.reg + 1

    val top = Counter().toED
    assertCodeString(
      top,
      """|class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val cnt = Bits(width) <> OUT
         |  val cnt_reg = Bits(width) <> VAR
         |  process(all):
         |    cnt := (cnt_reg.uint + d"${width}'1").bits
         |  process(clk):
         |    if (clk.rising)
         |      if (rst == 1) cnt_reg :== b"0".repeat(width)
         |      else cnt_reg :== cnt
         |    end if
         |end Counter
         |""".stripMargin
    )
  }
  test("Basic UInt Counter") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Falling)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class Counter(val width: Int <> CONST = 8) extends RTDesign(cfg):
      val cnt = UInt(width) <> OUT init 0
      cnt := cnt.reg + 1

    val top = Counter().toED
    assertCodeString(
      top,
      """|class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val cnt = UInt(width) <> OUT
         |  val cnt_reg = UInt(width) <> VAR
         |  process(all):
         |    cnt := cnt_reg + d"${width}'1"
         |  process(clk, rst):
         |    if (rst == 0) cnt_reg :== d"${width}'0"
         |    else if (clk.falling) cnt_reg :== cnt
         |end Counter
         |""".stripMargin
    )
  }
  test("Declaration with type operation") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Falling)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class Test(val width: Int <> CONST) extends DFDesign:
      val z = UInt.until(width) <> OUT
      z := 0

    val top = Test(8).toED
    assertCodeString(
      top,
      """|class Test(val width: Int <> CONST = 8) extends EDDesign:
         |  val z = UInt(clog2(width)) <> OUT
         |  process(all):
         |    z := d"${clog2(width)}'0"
         |end Test
         |""".stripMargin
    )
  }
  test("Inside conditional") {
    class Test() extends RTDesign:
      val c = Boolean <> IN
      val z = UInt(8) <> OUT init 0
      if (c)
        z := z.reg + 1

    val top = Test().toED
    assertCodeString(
      top,
      """|class Test extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val c = Boolean <> IN
         |  val z = UInt(8) <> OUT
         |  val z_reg = UInt(8) <> VAR
         |  process(all):
         |    if (c) z := z_reg + d"8'1"
         |  process(clk):
         |    if (clk.rising)
         |      if (rst == 1) z_reg :== d"8'0"
         |      else z_reg :== z
         |    end if
         |end Test
         |""".stripMargin
    )
  }
  test("REG declarations") {
    class Test() extends RTDesign:
      val c = Boolean <> IN
      val z = UInt(8) <> OUT.REG init 0
      val y = Bits(8) <> OUT.REG init all(0)
      y(0).din := 1
      if (c)
        z.din       := z + 1
        y(7, 4).din := all(1)
      else y.din := all(0)

    val top = Test().toED
    assertCodeString(
      top,
      """|class Test extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val c = Boolean <> IN
         |  val z = UInt(8) <> OUT
         |  val y = Bits(8) <> OUT
         |  val z_din = UInt(8) <> VAR
         |  val y_din = Bits(8) <> VAR
         |  process(all):
         |    z_din := z
         |    y_din := y
         |    y_din(0) := 1
         |    if (c)
         |      z_din := z + d"8'1"
         |      y_din(7, 4) := h"f"
         |    else y_din := h"00"
         |    end if
         |  process(clk):
         |    if (clk.rising)
         |      if (rst == 1)
         |        z :== d"8'0"
         |        y :== h"00"
         |      else
         |        z :== z_din
         |        y :== y_din
         |      end if
         |    end if
         |end Test
         |""".stripMargin
    )
  }
  test("DFMatch test case 1") {
    class Test extends RTDesign:
      val status = UInt(8) <> VAR
      status match
        case 0 =>

    val top = Test().toED
    assertCodeString(
      top,
      """|class Test extends EDDesign:
         |  val status = UInt(8) <> VAR
         |  process(all):
         |    status match
         |      case d"8'0" =>
         |    end match
         |end Test
         |""".stripMargin
    )
  }
  test("DFMatch test case 2") {
    class Test extends RTDesign:
      val status = UInt(8) <> VAR.REG
      status match
        case 0 =>

    val top = Test().toED
    assertCodeString(
      top,
      """|class Test extends EDDesign:
         |  val clk = Bit <> IN
         |  val rst = Bit <> IN
         |  val status = UInt(8) <> VAR
         |  val status_din = UInt(8) <> VAR
         |  process(all):
         |    status_din := status
         |    status match
         |      case d"8'0" =>
         |    end match
         |  process(clk):
         |    if (clk.rising) status :== status_din
         |end Test
         |""".stripMargin
    )
  }
  test("If + param test case") {
    class Test(val width: Int <> CONST) extends RTDesign():
      val c = Boolean     <> IN
      val v = Bits(width) <> VAR
      if (c) v(0) := 1

    val top = Test(2).toED
    assertCodeString(
      top,
      """|class Test(val width: Int <> CONST = 2) extends EDDesign:
         |  val c = Boolean <> IN
         |  val v = Bits(width) <> VAR
         |  process(all):
         |    if (c) v(0) := 1
         |end Test
         |""".stripMargin
    )
  }
end ToEDSpec
