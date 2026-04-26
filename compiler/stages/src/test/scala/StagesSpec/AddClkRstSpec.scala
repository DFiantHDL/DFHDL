package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.addClkRst
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class AddClkRstSpec extends StageSpec:
  test("Basic design clk and rst addition") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfgI")
      @hw.constraints.timing.reset()
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_cfgI() extends Clk
         |case class Rst_cfgI() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgI")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val internal = new RTDomain:
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic hierarchy, applied twice") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x.reg(1, init = ?)

    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class IDTop extends RTDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val id = ID()
      id.x <> x
      y    <> id.y

    val id = (new IDTop).addClkRst.addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = ?)
         |end ID
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class IDTop extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id = ID()
         |  id.x <> x
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Clk and rst already exist") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val clk = Clk      <> IN
      val rst = Rst      <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfgI")
      @hw.constraints.timing.reset()
      val internal = new RTDomain:
        val clk = Clk      <> IN
        val rst = Rst      <> IN
        val x   = SInt(16) <> IN
        val y   = SInt(16) <> OUT
        x <> y
      y := x
    end ID
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_cfgI() extends Clk
         |case class Rst_cfgI() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgI")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val internal = new RTDomain:
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No rst") {
    @hw.constraints.timing.clock(grpName = "cfgNoRst")
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfgNoRstI")
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfgNoRst() extends Clk
         |case class Clk_cfgNoRstI() extends Clk
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRst")
         |class ID extends RTDesign:
         |  val clk = Clk_cfgNoRst <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRstI")
         |  val internal = new RTDomain:
         |    val clk = Clk_cfgNoRstI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No clk and rst") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfgNoRstI")
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfgNoRstI() extends Clk
         |
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRstI")
         |  val internal = new RTDomain:
         |    val clk = Clk_cfgNoRstI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between design and internal related domain") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      self =>
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.related(self)
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.related(ID)
         |  val internal = new RTDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between internal related domains") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfgI")
      @hw.constraints.timing.reset()
      val internal1 = new RTDomain:
        outer =>
        @hw.constraints.timing.related(outer)
        val ii = new RTDomain:
          val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      @hw.constraints.timing.related(internal1)
      val internal2 = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        y <> internal1.ii.x + x
      y := x
    end ID
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_cfgI() extends Clk
         |case class Rst_cfgI() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgI")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val internal1 = new RTDomain:
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    @timing.related(internal1)
         |    val ii = new RTDomain:
         |      val x = SInt(16) <> IN
         |    end ii
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal1
         |  @timing.related(internal1)
         |  val internal2 = new RTDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> (internal1.ii.x + x)
         |  end internal2
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Explicit clk and rst are kept + constraints") {
    @hw.constraints.io(loc = "pinClk")
    @hw.constraints.timing.clock(rate = 50.MHz, grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.io(loc = "pinClk2")
      @hw.constraints.timing.clock(rate = 25.MHz, grpName = "cfgSlow")
      @hw.constraints.timing.reset()
      val internal = new RTDomain:
        val clk = Clk      <> IN
        val rst = Rst      <> IN
        val x   = SInt(16) <> IN
        val y   = SInt(16) <> OUT
        x <> y
      y := x
    end ID
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_cfgSlow() extends Clk
         |case class Rst_cfgSlow() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  @io(loc = "pinClk")
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 25.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgSlow")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val internal = new RTDomain:
         |    @io(loc = "pinClk2")
         |    val clk = Clk_cfgSlow <> IN
         |    val rst = Rst_cfgSlow <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Internal design generates clk and rst") {
    class ClkGen extends EDDesign:
      @hw.constraints.timing.clock(grpName = "cfg")
      @hw.constraints.timing.reset()
      val src = new RTDomain:
        val clk = Clk <> IN
        val rst = Rst <> IN
      @hw.constraints.timing.clock(grpName = "genCfg")
      @hw.constraints.timing.reset()
      val gen = new RTDomain:
        val clk = Clk <> OUT
        val rst = Rst <> OUT
      gen.clk <> src.clk.as(gen.Clk)
      gen.rst <> src.rst.as(gen.Rst)
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x      = SInt(16) <> IN
      val y      = SInt(16) <> OUT
      val clkGen = new ClkGen()
      @hw.constraints.timing.clock(grpName = "genCfg")
      @hw.constraints.timing.reset()
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_genCfg() extends Clk
         |case class Rst_genCfg() extends Rst
         |
         |class ClkGen extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val src = new RTDomain:
         |    val clk = Clk_cfg <> IN
         |    val rst = Rst_cfg <> IN
         |  end src
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "genCfg")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val gen = new RTDomain:
         |    val clk = Clk_genCfg <> OUT
         |    val rst = Rst_genCfg <> OUT
         |  end gen
         |  gen.clk <> src.clk.as(Clk_genCfg)
         |  gen.rst <> src.rst.as(Rst_genCfg)
         |end ClkGen
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val clkGen = ClkGen()
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "genCfg")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val internal = new RTDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
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

    val id = (new IDTop).addClkRst
    assertCodeString(
      id,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val dmn1 = new RTDomain:
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val dmn2 = new RTDomain:
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val id = ID()
         |    id.x <> dmn1.id.y.reg(1, init = sd"16'0")
         |  end dmn2
         |  @timing.related(dmn1)
         |  val dmn3 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y.reg(1, init = sd"16'0")
         |  end dmn3
         |  y <> dmn3.id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Derive config with `.norst`") {
    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.constraints.timing.clock(grpName = "cfg")
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |  val internal = new RTDomain:
         |    val clk = Clk_cfg <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  end internal
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Top-level clk/rst are VARs") {
    class FooChild extends RTDesign:
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Foo extends RTDesign:
      val clk = Clk <> VAR
      val rst = Rst <> VAR
      clk.actual := 0
      rst.actual := 0
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class FooChild extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class Foo extends RTDesign:
         |  val clk = Clk_cfg <> VAR
         |  val rst = Rst_cfg <> VAR
         |  clk.actual := 0
         |  rst.actual := 0
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation clk/rst generated") {
    class FooChild extends RTDesign:
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Foo extends RTDesign:
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class FooChild extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class Foo extends RTDesign:
         |  val clk = Clk_cfg <> VAR
         |  val rst = Rst_cfg <> VAR
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
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation internal clk/rst declared") {
    class FooChild extends RTDesign:
      val clk = Clk     <> IN
      val rst = Rst     <> IN
      val y   = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    @hw.constraints.timing.clock(grpName = "cfg")
    @hw.constraints.timing.reset()
    class Foo extends RTDesign:
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class FooChild extends RTDesign:
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfg")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class Foo extends RTDesign:
         |  val clk = Clk_cfg <> VAR
         |  val rst = Rst_cfg <> VAR
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
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation clk only generated") {
    @hw.constraints.timing.clock(grpName = "cfgNoRst")
    class FooChild extends RTDesign:
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    @hw.constraints.timing.clock(grpName = "cfgNoRst")
    class Foo extends RTDesign:
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfgNoRst() extends Clk
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRst")
         |class FooChild extends RTDesign:
         |  val clk = Clk_cfgNoRst <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRst")
         |class Foo extends RTDesign:
         |  val clk = Clk_cfgNoRst <> VAR
         |  @hw.annotation.flattenMode.transparent()
         |  val clkRstSimGen = new EDDomain:
         |    process:
         |      while (true)
         |        clk.actual :== 0
         |        10.ns.wait
         |        clk.actual :== 1
         |        10.ns.wait
         |      end while
         |  end clkRstSimGen
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation internal clk declared") {
    @hw.constraints.timing.clock(grpName = "cfgNoRst")
    class FooChild extends RTDesign:
      val clk = Clk     <> IN
      val y   = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    @hw.constraints.timing.clock(grpName = "cfgNoRst")
    class Foo extends RTDesign:
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfgNoRst() extends Clk
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRst")
         |class FooChild extends RTDesign:
         |  val clk = Clk_cfgNoRst <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "cfgNoRst")
         |class Foo extends RTDesign:
         |  val clk = Clk_cfgNoRst <> VAR
         |  @hw.annotation.flattenMode.transparent()
         |  val clkRstSimGen = new EDDomain:
         |    process:
         |      while (true)
         |        clk.actual :== 0
         |        10.ns.wait
         |        clk.actual :== 1
         |        10.ns.wait
         |      end while
         |  end clkRstSimGen
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
end AddClkRstSpec
