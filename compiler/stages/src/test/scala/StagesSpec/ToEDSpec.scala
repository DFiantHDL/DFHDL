package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.toED
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
//TODO: rethink blocking assignment in process(all) for VHDL vs. Verilog
//TODO: rethink rising_edge for VHDL vs. Verilog
class ToEDSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Basic wires and reg") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
  // a shared-variable (RAM) write lowers to `:==` like every other assignment in the clocked
  // process (`SanityCheck.sharedAssignCheck` rejects the blocking form there); backends then
  // render it per the target's object class (Verilog `<=`, VHDL `:=`)
  test("Shared variable writes become non-blocking in the clocked process") {
    class TrueDPR(
        val DATA_WIDTH: Int <> CONST = 8,
        val ADDR_WIDTH: Int <> CONST = 8
    ) extends EDDesign:
      val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED

      val a, b = new RTDomain:
        val data = Bits(DATA_WIDTH) <> IN
        val addr = Bits(ADDR_WIDTH) <> IN
        val q    = Bits(DATA_WIDTH) <> OUT.REG
        val we   = Bit              <> IN

        if (we)
          ram(addr) := data
        q.din       := ram(addr)
    end TrueDPR
    val top = (new TrueDPR()).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |
         |class TrueDPR(
         |    val DATA_WIDTH: Int <> CONST = 8,
         |    val ADDR_WIDTH: Int <> CONST = 8
         |) extends EDDesign:
         |  val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED
         |  val a = new EDDomain:
         |    @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |    val clk = Clk_default <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT
         |    val we = Bit <> IN
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (we) ram(addr.uint.toInt) :== data
         |        q :== ram(addr.uint.toInt)
         |      end if
         |  end a
         |  val b = new EDDomain:
         |    @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |    val clk = Clk_default <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT
         |    val we = Bit <> IN
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (we) ram(addr.uint.toInt) :== data
         |        q :== ram(addr.uint.toInt)
         |      end if
         |  end b
         |end TrueDPR
         |""".stripMargin
    )
  }
  test("Rising clk, Async Reset") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset(mode = _.async)
    class ID extends RTDesign:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
    @hw.constraints.timing.clock(grpName = "cfg", edge = _.falling)
    class ID extends RTDesign:
      val x  = SInt(16) <> IN
      val r1 = SInt(16) <> VAR
      r1 := x.reg(1, init = 0)
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.falling, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val r1 = SInt(16) <> VAR init sd"16'0"
         |  process(clk):
         |    if (clk.actual.falling) r1 :== x
         |end ID
         |""".stripMargin
    )
  }
  test("Rising clk, Sync Reset & Active-low") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset(mode = _.async, active = _.low)
    class ID extends RTDesign:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val r = SInt(16) <> VAR
      r := 1
      r := x + r
      y := r.reg(1, init = ?)

    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class IDTop extends RTDesign:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) {}
         |      else y :== y_din
         |    end if
         |end ID
         |
         |class IDTop extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Counter(val width: Int <> CONST = 8) extends RTDesign:
      val cnt = Bits(width) <> OUT init all(0)
      cnt := cnt.reg + 1

    val top = Counter().toED
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val cnt = Bits(width) <> OUT
         |  val cnt_reg = Bits(width) <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) cnt_reg :== b"0".repeat(width)
         |      else cnt_reg :== cnt
         |    end if
         |  cnt <> (cnt_reg.uint + d"1'1".resize(width)).bits
         |end Counter
         |""".stripMargin
    )
  }
  test("Basic UInt Counter") {
    @hw.constraints.timing.clock(grpName = "cfg", edge = _.falling)
    @hw.constraints.timing.reset(mode = _.async, active = _.low)
    class Counter(val width: Int <> CONST = 8) extends RTDesign:
      val cnt = UInt(width) <> OUT init 0
      cnt := cnt.reg + 1

    val top = Counter().toED
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Counter(val width: Int <> CONST = 8) extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.falling, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val cnt = UInt(width) <> OUT
         |  val cnt_reg = UInt(width) <> VAR
         |  process(clk, rst):
         |    if (rst.actual == 0) cnt_reg :== d"1'0".resize(width)
         |    else if (clk.actual.falling) cnt_reg :== cnt
         |  cnt <> (cnt_reg + d"1'1".resize(width))
         |end Counter
         |""".stripMargin
    )
  }
  test("Declaration with type operation") {
    class Test(val width: Int <> CONST) extends DFDesign:
      val z = UInt.until(width) <> OUT
      z := 0

    val top = Test(8).toED
    assertCodeString(
      top,
      """|class Test(val width: Int <> CONST = 8) extends EDDesign:
         |  val z = UInt(clog2(width)) <> OUT
         |  z <> d"1'0".resize(clog2(width))
         |end Test
         |""".stripMargin
    )
  }
  test("Inside conditional") {
    class Test() extends RTDesign:
      val c = Boolean <> IN
      val z = UInt(8) <> OUT init 0
      z := 0
      if (c)
        z := z.reg + 1

    val top = Test().toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Test extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val c = Boolean <> IN
         |  val z = UInt(8) <> OUT
         |  val z_ver_reg = UInt(8) <> VAR
         |  val z_ver_reg_din = UInt(8) <> VAR
         |  process(all):
         |    z_ver_reg_din := z_ver_reg
         |    z := d"8'0"
         |    if (c)
         |      z_ver_reg_din := z
         |      z := z_ver_reg + d"8'1"
         |    end if
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) z_ver_reg :== d"8'0"
         |      else z_ver_reg :== z_ver_reg_din
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
        z.din := z + 1
        assert(z == 77, s"y: $y")
        y(7, 4).din := all(1)
      else y.din := all(0)

    val top = Test().toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Test extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
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
         |          assert(z == d"8'77", s"y: ${y}")
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
      val y      = Bit     <> OUT
      val status = UInt(8) <> VAR
      y := 1
      status match
        case 0 => y := 0

    val top = Test().toED
    assertCodeString(
      top,
      """|class Test extends EDDesign:
         |  val y = Bit <> OUT
         |  val status = UInt(8) <> VAR
         |  process(all):
         |    y := 1
         |    status match
         |      case d"8'0" => y := 0
         |    end match
         |end Test
         |""".stripMargin
    )
  }
  test("DFMatch test case 2") {
    class Test extends RTDesign:
      val y      = Bit     <> OUT
      val status = UInt(8) <> VAR.REG
      y := 1
      status match
        case 0 => y := 0

    val top = Test().toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |
         |class Test extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val y = Bit <> OUT
         |  val status = UInt(8) <> VAR
         |  val status_din = UInt(8) <> VAR
         |  process(all):
         |    y := 1
         |    status match
         |      case d"8'0" => y := 0
         |    end match
         |  process(clk):
         |    if (clk.actual.rising) status :== status_din
         |end Test
         |""".stripMargin
    )
  }
  test("If + param test case") {
    class Test(val width: Int <> CONST) extends RTDesign:
      val c = Boolean     <> IN
      val v = Bits(width) <> VAR
      v           := all(0)
      if (c) v(0) := 1

    val top = Test(2).toED
    assertCodeString(
      top,
      """|class Test(val width: Int <> CONST = 2) extends EDDesign:
         |  val c = Boolean <> IN
         |  val v = Bits(width) <> VAR
         |  process(all):
         |    v := b"0".repeat(width)
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
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
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y.reg(1, init = 0)
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
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
         |    @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  val dmn2 = new EDDomain:
         |    @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
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
         |  end dmn2
         |  val dmn3 = new EDDomain:
         |    val dmn2_id_y_reg = SInt(16) <> VAR
         |    val id = ID()
         |    id.x <> dmn2_id_y_reg
         |    process(dmn1.clk):
         |      if (dmn1.clk.actual.rising)
         |        if (dmn1.rst.actual == 1) dmn2_id_y_reg :== sd"16'0"
         |        else dmn2_id_y_reg :== dmn2.id.y
         |      end if
         |  end dmn3
         |  y <> dmn3.id.y
         |end IDTop
         |""".stripMargin
    )
  }

  test("RT domain with basic combinational if-else") {
    class IDTop extends EDDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
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
         |  end dmn1
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
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
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      self =>
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0
      val r = SInt(16) <> VAR.REG init 0
      @hw.constraints.timing.related(self)
      val foo = new RTDomain:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
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
         |  end foo
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== sd"16'0"
         |      else r :== sd"16'1"
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("related domain with includeReset = false relies on init instead of reset") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      self =>
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0
      @hw.constraints.timing.related(self, includeReset = false)
      val foo = new RTDomain:
        val z = SInt(16) <> OUT.REG init 0
        z.din := x
      y.din := x
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val foo = new EDDomain:
         |    val z = SInt(16) <> OUT init sd"16'0"
         |    process(clk):
         |      if (clk.actual.rising) z :== x
         |  end foo
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) y :== sd"16'0"
         |      else y :== x
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
      self =>
      val regs = Bits(DATA_WIDTH) X REG_NUM <> VAR.REG

      @hw.constraints.timing.related(self)
      val rs1, rs2 = new RTDomain:
        val addr = Bits.until(REG_NUM) <> IN
        val data = Bits(DATA_WIDTH)    <> OUT.REG
        data.din := regs(addr)

      @hw.constraints.timing.related(self)
      val rd = new RTDomain:
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val regs = Bits(DATA_WIDTH) X REG_NUM <> VAR
         |  val rs1 = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> OUT
         |    process(clk):
         |      if (clk.actual.rising) data :== regs(addr.uint.toInt)
         |  end rs1
         |  val rs2 = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> OUT
         |    process(clk):
         |      if (clk.actual.rising) data :== regs(addr.uint.toInt)
         |  end rs2
         |  val rd = new EDDomain:
         |    val addr = Bits(clog2(REG_NUM)) <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val wren = Bit <> IN
         |    process(clk):
         |      if (clk.actual.rising)
         |        if (wren) regs(addr.uint.toInt) :== data
         |        regs(0) :== b"0".repeat(DATA_WIDTH)
         |      end if
         |  end rd
         |end RegFile
         |""".stripMargin
    )
  }

  test("left-right shift example") {
    enum ShiftDir extends Encoded:
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
      """|enum ShiftDir(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
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

  test("RT design with ED domain") {
    class Foo extends RTDesign:
      val clk      = Clk <> VAR
      val rst      = Rst <> VAR
      val internal = new EDDomain:
        process(all):
          clk.actual := 0
          rst.actual := 0
      val y = UInt(8) <> VAR.REG init d"8'0"
      y.din := y + d"8'1"
    end Foo
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|class Foo extends EDDesign:
         |  case class Clk_default() extends Clk
         |  case class Rst_default() extends Rst
         |
         |  val clk = Clk_default <> VAR
         |  val rst = Rst_default <> VAR
         |  val y = UInt(8) <> VAR
         |  val internal = new EDDomain:
         |    process(all):
         |      clk.actual := 0
         |      rst.actual := 0
         |  end internal
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) y :== d"8'0"
         |      else y :== y + d"8'1"
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("Printing internal design port") {
    class FooChild extends RTDesign:
      val clk = Clk     <> IN
      val rst = Rst     <> IN
      val y   = UInt(8) <> OUT.REG init d"8'0"
      y.din := y + 1
    end FooChild

    class Foo extends RTDesign:
      val clk   = Clk <> IN
      val rst   = Rst <> IN
      val child = FooChild()
      println(s"${child.y}")
    end Foo
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class FooChild extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val y = UInt(8) <> OUT
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) y :== d"8'0"
         |      else y :== y + d"8'1"
         |    end if
         |end FooChild
         |
         |class Foo extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val child = FooChild()
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) {}
         |      else println(s"${child.y}")
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("For loop with a register") {
    class Foo(
        val PORT_WIDTH: Int <> CONST = 5
    ) extends RTDesign:
      val r = Bits(PORT_WIDTH) <> OUT.REG init all(0)
      COMB_LOOP:
        for (i <- 0 until PORT_WIDTH)
          r(i).din := 1
        for (i <- 0 until PORT_WIDTH)
          if (r(PORT_WIDTH - 1 - i))
            r(i).din := 0
    end Foo

    val top = (new Foo()).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Foo(val PORT_WIDTH: Int <> CONST = 5) extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val r = Bits(PORT_WIDTH) <> OUT
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== b"0".repeat(PORT_WIDTH)
         |      else
         |        for (i <- 0 until PORT_WIDTH)
         |          r(i) :== 1
         |        end for
         |        for (i <- 0 until PORT_WIDTH)
         |          if (r((PORT_WIDTH - 1) - i)) r(i) :== 0
         |        end for
         |      end if
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("For loop with a register and combinational loop") {
    class Foo(
        val PORT_WIDTH: Int <> CONST = 5
    ) extends RTDesign:
      val r = Bits(PORT_WIDTH) <> OUT.REG init all(0)
      val w = Bits(PORT_WIDTH) <> OUT
      COMB_LOOP:
        for (i <- 0 until PORT_WIDTH)
          r(i).din := 1
        for (i <- 0 until PORT_WIDTH)
          if (r(PORT_WIDTH - 1 - i))
            r(i).din := 0
        for (i <- 0 until PORT_WIDTH)
          w(i) := r(i)
    end Foo

    val top = (new Foo()).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Foo(val PORT_WIDTH: Int <> CONST = 5) extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val r = Bits(PORT_WIDTH) <> OUT
         |  val w = Bits(PORT_WIDTH) <> OUT
         |  val r_din = Bits(PORT_WIDTH) <> VAR
         |  process(all):
         |    r_din := r
         |    for (i <- 0 until PORT_WIDTH)
         |      r_din(i) := 1
         |    end for
         |    for (i <- 0 until PORT_WIDTH)
         |      if (r((PORT_WIDTH - 1) - i)) r_din(i) := 0
         |    end for
         |    for (i <- 0 until PORT_WIDTH)
         |      w(i) := r(i)
         |    end for
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== b"0".repeat(PORT_WIDTH)
         |      else r :== r_din
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("match inside if") {
    class Foo extends RTDesign:
      enum State extends Encoded:
        case S0
      val x     = Bit   <> IN
      val state = State <> VAR.REG init State.S0
      if (x) state.din := State.S0
      else
        state match
          case State.S0 => state.din := State.S0
        end match
      end if
    end Foo
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Foo extends EDDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val x = Bit <> IN
         |  val state = State <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) state :== State.S0
         |      else
         |        if (x) state :== State.S0
         |        else
         |          state match
         |            case State.S0 => state :== State.S0
         |          end match
         |        end if
         |      end if
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("for loop inside if") {
    class Foo extends RTDesign:
      val z      = Bit         <> IN
      val x      = UInt(8) X 7 <> VAR.REG
      val FooStr = "Hello World!".toByteVector
      if (z)
        COMB_LOOP:
          for (i <- 0 until 7)
            x(i).din := FooStr(i)
    end Foo
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |
         |class Foo extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val FooStr: Bits[8] X 12 <> CONST = DFVector(Bits(8) X 12)(h"48", h"65", h"6c", h"6c", h"6f", h"20", h"57", h"6f", h"72", h"6c", h"64", h"21")
         |  val z = Bit <> IN
         |  val x = UInt(8) X 7 <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (z)
         |        for (i <- 0 until 7)
         |          x(i) :== FooStr(i).uint
         |        end for
         |      end if
         |    end if
         |end Foo""".stripMargin
    )
  }

  test("regression check: a design with a single register") {
    class Foo extends RTDesign:
      val counter = Int <> VAR.REG init 0
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|class Foo extends EDDesign:
         |  case class Clk_default() extends Clk
         |  case class Rst_default() extends Rst
         |
         |  val clk = Clk_default <> VAR
         |  val rst = Rst_default <> VAR
         |  @hw.annotation.flattenMode.transparent()
         |  val clkRstSimGen = new EDDomain:
         |    process:
         |      rst.actual :== 1
         |      while (true)
         |        clk.actual :== 0
         |        10.ns.wait
         |        clk.actual :== 1
         |        10.ns.wait
         |        rst.actual :== 0
         |      end while
         |  end clkRstSimGen
         |  val counter = Int <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) counter :== 0
         |      else {}
         |    end if
         |end Foo""".stripMargin
    )
  }
  test("initial block planted into the reset branch") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Init extends RTDesign:
      val x   = SInt(16)     <> IN
      val y   = SInt(16)     <> OUT.REG
      val vec = SInt(16) X 4 <> VAR.REG
      initial:
        for (i <- 0 until 4)
          vec(i).din := 0
      y.din      := x + vec(0)
      vec(1).din := x
    end Init
    val top = (new Init).toED
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Init extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val vec = SInt(16) X 4 <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        for (i <- 0 until 4)
         |          vec(i) :== sd"16'0"
         |        end for
         |      else
         |        y :== x + vec(0)
         |        vec(1) :== x
         |      end if
         |    end if
         |end Init
         |""".stripMargin
    )
  }
  test("initial block without a reset passes through as an ED initial block") {
    class Init extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val v = SInt(16) <> VAR
      initial:
        v := 1
      y := x + v
    end Init
    val top = (new Init).toED
    assertCodeString(
      top,
      """|class Init extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  initial:
         |    v := sd"16'1"
         |  y <> (x + v)
         |end Init
         |""".stripMargin
    )
  }
  test("for-loop process start costs no bootstrap cycle") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Foo extends RTDesign:
      val y = SInt(16) <> OUT.REG
      process:
        for (i <- 0 until 4)
          y.din := i
          1.cy.wait
    end Foo
    val top = (new Foo).toED
    // the payoff of the initial-block lowering plus first-step fusion: the loop control step
    // fuses into the wait's exit site (forwarded `(i + 1) < 4` guard) and the reset-site fold
    // drops the bootstrap state, so reset provides the iterator and first output values
    // directly (via the generated initial block planted into the reset branch) and every loop
    // iteration costs exactly its one wait cycle, including the forever wrap-around
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class Foo extends EDDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S_0_0 extends State(d"1'0")
         |
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = SInt(16) <> OUT
         |  val i = Int <> VAR
         |  val state = State <> VAR
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        y :== sd"16'${0}"
         |        i :== 0
         |        state :== State.S_0_0
         |      else
         |        state match
         |          case State.S_0_0 =>
         |            i :== i + 1
         |            if ((i + 1) < 4)
         |              y :== sd"16'${(i + 1)}"
         |              state :== State.S_0_0
         |            else
         |              i :== 0
         |              y :== sd"16'${0}"
         |              state :== State.S_0_0
         |            end if
         |        end match
         |      end if
         |    end if
         |end Foo
         |""".stripMargin
    )
  }
  // the constant Boolean guard is inlined at elaboration (a design body has no ambient
  // conditional-statement capability, so `if (F)` is evaluated as a Scala `if`), leaving
  // only the taken branch in the design
  test("local param is not dragged within a sync process") {
    class Foo extends RTDesign:
      val F: Boolean <> CONST = false
      val o                   = Bit <> OUT.REG init 0
      if (F) o.din := 1
      else o.din   := 0
    end Foo
    val top = (new Foo).toED
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class Foo extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val F: Boolean <> CONST = false
         |  val o = Bit <> OUT
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) o :== 0
         |      else o :== 0
         |    end if
         |end Foo
         |""".stripMargin
    )
  }
  test("REG DIN read") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val r = UInt(8) <> VAR.REG init 0
      val y = UInt(8) <> OUT
      r.din := r.din + d"8'1"
      r.din := r.din + d"8'1"
      y     := r
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT
         |  val r = UInt(8) <> VAR
         |  val r_din = UInt(8) <> VAR
         |  process(all):
         |    r_din := r
         |    r_din := r_din + d"8'1"
         |    r_din := r_din + d"8'1"
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== d"8'0"
         |      else r :== r_din
         |    end if
         |  y <> r
         |end ID
         |""".stripMargin
    )
  }
  test("REG DIN partial read") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val r = UInt(8) <> VAR.REG init 0
      val y = UInt(4) <> OUT
      r(3, 0).din := d"4'5"
      y           := r(3, 0).din
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(4) <> OUT
         |  val r = UInt(8) <> VAR
         |  val r_din = UInt(8) <> VAR
         |  process(all):
         |    r_din := r
         |    r_din(3, 0) := d"4'5"
         |    y := r_din(3, 0)
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== d"8'0"
         |      else r :== r_din
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("REG DIN read without a DIN assignment") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val r = UInt(8) <> VAR.REG init 0
      val y = UInt(8) <> OUT
      y := r.din
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT
         |  val r = UInt(8) <> VAR
         |  val r_din = UInt(8) <> VAR
         |  process(all):
         |    r_din := r
         |    y := r_din
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== d"8'0"
         |      else r :== r_din
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("REG DIN read forces a combinational process in a sequential domain") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x  = UInt(8) <> IN
      val r1 = UInt(8) <> VAR.REG init 0
      val r2 = UInt(8) <> VAR.REG init 0
      // r2 has no DIN read, so it must not gain a default assignment of its own
      r1.din := r1.din + x
      r2.din := x
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = UInt(8) <> IN
         |  val r1 = UInt(8) <> VAR
         |  val r2 = UInt(8) <> VAR
         |  val r1_din = UInt(8) <> VAR
         |  val r2_din = UInt(8) <> VAR
         |  process(all):
         |    r1_din := r1
         |    r1_din := r1_din + x
         |    r2_din := x
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1)
         |        r1 :== d"8'0"
         |        r2 :== d"8'0"
         |      else
         |        r1 :== r1_din
         |        r2 :== r2_din
         |      end if
         |    end if
         |end ID
         |""".stripMargin
    )
  }
  test("REG DIN read is not hoisted out of the process") {
    // A DIN read yields the pending value AT ITS POSITION. Promoting `sum` to a concurrent
    // connection (which its single assignment would otherwise earn) would make it read the
    // shadow's final value instead, closing a combinational loop: `sum` feeds `r_din` and
    // would then also be computed from it. `sum` must stay in the process, before `r_din := sum`.
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val r   = UInt(8) <> VAR.REG init 0
      val y   = UInt(8) <> OUT
      val sum = r.din + d"8'1"
      r.din := sum
      y     := sum
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT
         |  val r = UInt(8) <> VAR
         |  val sum = UInt(8) <> VAR
         |  val r_din = UInt(8) <> VAR
         |  process(all):
         |    r_din := r
         |    sum := r_din + d"8'1"
         |    r_din := sum
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== d"8'0"
         |      else r :== r_din
         |    end if
         |  y <> sum
         |end ID
         |""".stripMargin
    )
  }
  test("REG DIN read under VHDL uses a process variable") {
    // VHDL signal assignment evaluates every RHS against the pre-process value, so a shadow
    // SIGNAL would turn `r_din := r_din + 1` twice into a single increment, and being
    // self-referential inside `process(all)` it would never settle. The shadow is therefore a
    // process variable, published to the design-level signal that the clocked process reads.
    given options.CompilerOptions.Backend = _.vhdl.v2008
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val r = UInt(8) <> VAR.REG init 0
      val y = UInt(8) <> OUT
      r.din := r.din + d"8'1"
      r.din := r.din + d"8'1"
      y     := r
    end ID
    val id = (new ID).toED
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "cfg")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT
         |  val r = UInt(8) <> VAR
         |  val r_din = UInt(8) <> VAR
         |  process(all):
         |    val r_din_v = UInt(8) <> VAR
         |    r_din_v := r
         |    r_din_v := r_din_v + d"8'1"
         |    r_din_v := r_din_v + d"8'1"
         |    r_din :== r_din_v
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) r :== d"8'0"
         |      else r :== r_din
         |    end if
         |  y <> r
         |end ID
         |""".stripMargin
    )
  }
end ToEDSpec
