package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.toED
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
//TODO: rethink blocking assignment in process(all) for VHDL vs. Verilog
//TODO: rethink rising_edge for VHDL vs. Verilog
class ToEDSpec extends StageSpec(stageCreatesUnrefAnons = true):
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
      val r2 = SInt(16) <> VAR.REG init 0
      val r3 = Bits(16) <> VAR.REG init all(0)
      w1 := x
      w1 := w1 + 1
      w2 := x
      r1 := w2.reg(1, init = 0)
      if (x > 0)
        r2.din     := x
      r3(7, 0).din := h"88"
      y            := w1 + r1
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val w1 = SInt(16) <> VAR
         |  val w2 = SInt(16) <> VAR
         |  val r1 = SInt(16) <> VAR
         |  val r2 = SInt(16) <> VAR
         |  val r3 = Bits(16) <> VAR
         |  val r1_din = SInt(16) <> VAR
         |  val r2_din = SInt(16) <> VAR
         |  val r3_din = Bits(16) <> VAR
         |  process(all):
         |    r2_din := r2
         |    r3_din := r3
         |    w1 := x
         |    w1 := w1 + sd"16'1"
         |    r1_din := w2
         |    if (x > sd"16'0") r2_din := x
         |    r3_din(7, 0) := h"88"
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        r1 :== sd"16'0"
         |        r2 :== sd"16'0"
         |        r3 :== h"0000"
         |      else
         |        r1 :== r1_din
         |        r2 :== r2_din
         |        r3 :== r3_din
         |      end if
         |    end if
         |  w2 <> x
         |  y <> (w1 + r1)
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
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk, rst):
         |    if (rst.actual == 1) r1 :== sd"16'0"
         |    else if (clk.actual.rising) r1 :== x
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
      """|case class Clk_cfg() extends Clk
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk):
         |    if (clk.actual.falling) r1 :== x
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
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR
         |  process(clk, rst):
         |    if (rst.actual == 0) r1 :== sd"16'0"
         |    else if (clk.actual.rising) r1 :== x
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
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val r = SInt(16) <> VAR
         |  val y_din = SInt(16) <> VAR
         |  process(all):
         |    r := sd"16'1"
         |    r := x + r
         |    y_din := r
         |  process(clk):
         |    if (clk.actual.rising) y :== y_din
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val temp = SInt(16) <> VAR
         |  val id = ID()
         |  id.x <> temp
         |  process(all):
         |    temp := x
         |    if (x > sd"16'0") temp := temp + sd"16'1"
         |  y <> id.y
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
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val cnt = Bits(width) <> OUT
         |  val cnt_reg = Bits(width) <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) cnt_reg :== b"0".repeat(width)
         |      else cnt_reg :== cnt
         |    end if
         |  cnt <> (cnt_reg.uint + d"${width}'1").bits
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
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val cnt = UInt(width) <> OUT
         |  val cnt_reg = UInt(width) <> VAR
         |  process(clk, rst):
         |    if (rst.actual == 0) cnt_reg :== d"${width}'0"
         |    else if (clk.actual.falling) cnt_reg :== cnt
         |  cnt <> (cnt_reg + d"${width}'1")
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
         |  z <> d"${clog2(width)}'0"
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
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Test extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val c = Boolean <> IN
         |  val z = UInt(8) <> OUT
         |  val z_reg = UInt(8) <> VAR
         |  val z_reg_din = UInt(8) <> VAR
         |  process(all):
         |    z_reg_din := z
         |    if (c) z := z_reg + d"8'1"
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) z_reg :== d"8'0"
         |      else z_reg :== z_reg_din
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
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Test extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val c = Boolean <> IN
         |  val z = UInt(8) <> OUT
         |  val y = Bits(8) <> OUT
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        z :== d"8'0"
         |        y :== h"00"
         |      else
         |        y(0) :== 1
         |        if (c)
         |          z :== z + d"8'1"
         |          y(7, 4) :== h"f"
         |        else y :== h"00"
         |        end if
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
      """|case class Clk_default() extends Clk
         |
         |class Test extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val status = UInt(8) <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      status match
         |        case d"8'0" =>
         |      end match
         |    end if
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
  test("Basic hierarchy with regs on outputs") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends RTDesign:
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val id1 = ID()
      id1.x <> x.reg(1, init = 0)
      val id2 = ID()
      id2.x <> id1.y.reg(1, init = 0)
      y     <> id2.y

    val id = (new IDTop).toED
    assertCodeString(
      id,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y <> x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val x_reg = SInt(16) <> VAR
         |  val id1_y_reg = SInt(16) <> VAR
         |  val id1 = ID()
         |  val id2 = ID()
         |  id1.x <> x_reg
         |  id2.x <> id1_y_reg
         |  y <> id2.y
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        x_reg :== sd"16'0"
         |        id1_y_reg :== sd"16'0"
         |      else
         |        x_reg :== x
         |        id1_y_reg :== id1.y
         |      end if
         |    end if
         |end IDTop
         |""".stripMargin
    )
  }

  test("Basic hierarchy with domains") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y.reg(1, init = 0)
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).toED
    assertCodeString(
      id,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y <> x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new EDDomain:
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new EDDomain:
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val dmn1_id_y_reg = SInt(16) <> VAR
         |    val id = ID()
         |    id.x <> dmn1_id_y_reg
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (rst.actual == 1) dmn1_id_y_reg :== sd"16'0"
         |        else dmn1_id_y_reg :== dmn1.id.y
         |      end if
         |  val dmn3 = new EDDomain:
         |    val dmn2_id_y_reg = SInt(16) <> VAR
         |    val id = ID()
         |    id.x <> dmn2_id_y_reg
         |    process(dmn1.clk):
         |      if (dmn1.clk.actual.rising)
         |        if (dmn1.rst.actual == 1) dmn2_id_y_reg :== sd"16'0"
         |        else dmn2_id_y_reg :== dmn2.id.y
         |      end if
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }

  test("RT domain with basic combinational if-else") {
    class IDTop extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        if (x < 0) y := 0
        else y       := x

    val id = (new IDTop).toED
    assertCodeString(
      id,
      """|class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new EDDomain:
         |    process(all):
         |      if (x < sd"16'0") y := sd"16'0"
         |      else y := x
         |end IDTop
         |""".stripMargin
    )
  }

  test("a single register with only init") {
    class IDTop extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0

    val id = (new IDTop).toED
    assertCodeString(
      id,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class IDTop extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) y :== sd"16'0"
         |      else {}
         |    end if
         |end IDTop
         |""".stripMargin
    )
  }
  test("related domain uses external REG Dcls") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign(cfg):
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0
      val r = SInt(16) <> VAR.REG init 0
      val foo = new RelatedDomain:
        y.din := r
      r.din := 1
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val r = SInt(16) <> VAR
         |  val foo = new EDDomain:
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (rst.actual == 1) y :== sd"16'0"
         |        else y :== r
         |      end if
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== sd"16'0"
         |      else r :== sd"16'1"
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("register file example") {
    class RegFile(
        val DATA_WIDTH: Int <> CONST = 32,
        val REG_NUM: Int <> CONST    = 32
    ) extends RTDesign:
      val regs = Bits(DATA_WIDTH) X REG_NUM <> VAR.REG

      val rs1, rs2 = new RelatedDomain:
        val addr = Bits.until(REG_NUM) <> IN
        val data = Bits(DATA_WIDTH)    <> OUT.REG
        data.din := regs(addr)

      val rd = new RelatedDomain:
        val addr = Bits.until(REG_NUM) <> IN
        val data = Bits(DATA_WIDTH)    <> IN
        val wren = Bit                 <> IN
        if (wren) regs(addr).din := data
        regs(0).din              := all(0)
    end RegFile

    val top = (new RegFile).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |
         |class RegFile(
         |    val DATA_WIDTH: Int <> CONST = 32,
         |    val REG_NUM: Int <> CONST = 32
         |) extends EDDesign:
         |  val clk = Clk_default <> IN
         |  val regs = Bits(DATA_WIDTH) X REG_NUM <> VAR
         |  val rs1 = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> OUT
         |    process(clk):
         |      if (clk.actual.rising) data :== regs(addr.uint.toInt)
         |  val rs2 = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> OUT
         |    process(clk):
         |      if (clk.actual.rising) data :== regs(addr.uint.toInt)
         |  val rd = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val wren = Bit <> IN
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (wren) regs(addr.uint.toInt) :== data
         |        regs(0) :== b"0".repeat(DATA_WIDTH)
         |      end if
         |end RegFile
         |""".stripMargin
    )
  }

  test("left-right shift example") {
    enum ShiftDir extends Encode:
      case Left, Right

    class ShiftGen(val width: Int) extends RTDesign:
      val iBits = Bits(width)       <> IN
      val shift = UInt.until(width) <> IN
      val oBits = Bits(width)       <> OUT

    class LeftShiftGen(width: Int) extends ShiftGen(width):
      oBits := iBits << shift

    class RightShiftGen(width: Int) extends ShiftGen(width):
      oBits := iBits >> shift

    class LRShiftGen(width: Int = 8) extends ShiftGen(width):
      val dir      = ShiftDir <> IN
      val lshifter = LeftShiftGen(width)
      val rshifter = RightShiftGen(width)
      lshifter.iBits <> iBits
      lshifter.shift <> shift
      rshifter.iBits <> iBits
      rshifter.shift <> shift
      dir match
        case ShiftDir.Left  => oBits := lshifter.oBits
        case ShiftDir.Right => oBits := rshifter.oBits

    val top = (new LRShiftGen).toED
    assertCodeString(
      top,
      """|enum ShiftDir(val value: UInt[1] <> CONST) extends Encode.Manual(1):
         |  case Left extends ShiftDir(d"1'0")
         |  case Right extends ShiftDir(d"1'1")
         |
         |class LeftShiftGen extends EDDesign:
         |  val iBits = Bits(8) <> IN
         |  val shift = UInt(3) <> IN
         |  val oBits = Bits(8) <> OUT
         |  oBits <> (iBits << shift.toInt)
         |end LeftShiftGen
         |
         |class RightShiftGen extends EDDesign:
         |  val iBits = Bits(8) <> IN
         |  val shift = UInt(3) <> IN
         |  val oBits = Bits(8) <> OUT
         |  oBits <> (iBits >> shift.toInt)
         |end RightShiftGen
         |
         |class LRShiftGen extends EDDesign:
         |  val iBits = Bits(8) <> IN
         |  val shift = UInt(3) <> IN
         |  val oBits = Bits(8) <> OUT
         |  val dir = ShiftDir <> IN
         |  val lshifter = LeftShiftGen()
         |  val rshifter = RightShiftGen()
         |  lshifter.iBits <> iBits
         |  lshifter.shift <> shift
         |  rshifter.iBits <> iBits
         |  rshifter.shift <> shift
         |  process(all):
         |    dir match
         |      case ShiftDir.Left => oBits := lshifter.oBits
         |      case ShiftDir.Right => oBits := rshifter.oBits
         |    end match
         |end LRShiftGen""".stripMargin
    )
  }

  test("Basic hierarchy design with parameters") {
    class ID(val width: Int <> CONST) extends DFDesign:
      val x = SInt(width) <> IN
      val y = SInt(width) <> OUT
      y := x

    class IDTop(val width: Int <> CONST) extends DFDesign:
      val x   = SInt(width) <> IN
      val y   = SInt(width) <> OUT
      val id1 = ID(width)
      val id2 = ID(width)
      id1.x <> x
      id1.y <> id2.x
      id2.y <> y
    val top = (new IDTop(16)).toED
    assertCodeString(
      top,
      """|class ID(val width: Int <> CONST) extends EDDesign:
         |  val x = SInt(width) <> IN
         |  val y = SInt(width) <> OUT
         |  y <> x
         |end ID
         |
         |class IDTop(val width: Int <> CONST = 16) extends EDDesign:
         |  val x = SInt(width) <> IN
         |  val y = SInt(width) <> OUT
         |  val id1 = ID(width = width)
         |  val id2 = ID(width = width)
         |  id1.x <> x
         |  id2.x <> id1.y
         |  y <> id2.y
         |end IDTop""".stripMargin
    )
  }

end ToEDSpec
