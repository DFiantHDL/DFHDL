package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitClkRstCfg
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitClkRstCfgSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Basic hierarchy, combinational") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends RTDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val id = ID()
      id.x <> x
      y    <> id.y

    val id = (new IDTop).explicitClkRstCfg
    // Comb designs get no @timing.clock / @timing.reset annotations,
    // so `class ID extends RTDesign:` is the expected shape.
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id = ID()
         |  id.x <> x
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy, combinational, always include clock and reset at top") {
    given options.ElaborationOptions.DefaultClkCfg =
      hw.constraints.timing.clock(inclusionPolicy =
        hw.constraints.timing.InclusionPolicy.AlwaysAtTop
      )
    given options.ElaborationOptions.DefaultRstCfg =
      hw.constraints.timing.reset(inclusionPolicy =
        hw.constraints.timing.InclusionPolicy.AlwaysAtTop
      )
    val eo = summon[options.ElaborationOptions]
    // force DFC with these elaboration options modifications (this is required because no @top annotation)
    val dfc                               = DFC.empty(eo)
    def gen(using DFC): dfhdl.core.Design =
      class ID extends RTDesign:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        y := x

      class IDTop extends RTDesign:
        val x  = SInt(16) <> IN
        val y  = SInt(16) <> OUT
        val id = ID()
        id.x <> x
        y    <> id.y

      new IDTop
    val id = (gen(using dfc)).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AlwaysAtTop, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AlwaysAtTop)
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id = ID()
         |  id.x <> x
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy, registered") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x.reg(1, init = 0)

    class IDTop extends RTDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val id = ID()
      id.x <> x
      y    <> id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = sd"16'0")
         |end ID
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id = ID()
         |  id.x <> x
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy with domains combinational, top domain = ED") {
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
        id.x <> dmn1.id.y
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
        val id = ID()
        id.x <> dmn2.id.y
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain:
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  val dmn2 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  end dmn2
         |  @timing.related(dmn1)
         |  val dmn3 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y
         |  end dmn3
         |  y <> dmn3.id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy with domains registered, top domain = ED") {
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
        id.x <> dmn1.id.y
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
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
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  val dmn2 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
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
  test("Basic hierarchy with domains combinational, top domain = RT") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends RTDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
        val id = ID()
        id.x <> dmn2.id.y
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.related(IDTop)
         |  val dmn1 = new RTDomain:
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  @timing.related(IDTop)
         |  val dmn2 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  end dmn2
         |  @timing.related(dmn1)
         |  val dmn3 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y
         |  end dmn3
         |  y <> dmn3.id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy with domains registered, top domain = RT") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends RTDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.related(IDTop)
         |  val dmn1 = new RTDomain:
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  @timing.related(IDTop)
         |  val dmn2 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
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
  test("Basic hierarchy with domains registered, top domain = RT, custom config") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x.reg(1, init = 5)

    class IDTop extends RTDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      @hw.constraints.timing.related(dmn1)
      val dmn3 = new RTDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    // TODO: figure out why no different ID designs with different configurations are created,
    // and why ID also picks up the resolved timing annotations of its enclosing IDTop.
    assertCodeString(
      id,
      """|@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = sd"16'5")
         |end ID
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class IDTop extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @timing.related(IDTop)
         |  val dmn1 = new RTDomain:
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  @timing.related(IDTop)
         |  val dmn2 = new RTDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
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
  test("TrueDPR example") {
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

    val top = (new TrueDPR()).explicitClkRstCfg
    assertCodeString(
      top,
      """|class TrueDPR(
         |    val DATA_WIDTH: Int <> CONST = 8,
         |    val ADDR_WIDTH: Int <> CONST = 8
         |) extends EDDesign:
         |  val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  val a = new RTDomain:
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT.REG
         |    val we = Bit <> IN
         |    if (we) ram(addr.uint.toInt) := data
         |    q.din := ram(addr.uint.toInt)
         |  end a
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  val b = new RTDomain:
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT.REG
         |    val we = Bit <> IN
         |    if (we) ram(addr.uint.toInt) := data
         |    q.din := ram(addr.uint.toInt)
         |  end b
         |end TrueDPR
         |""".stripMargin
    )
  }
  test("Regs with bubble init have no reset") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init ?
      y.din := x.reg(1, init = ?)

    val id = (new ID).explicitClkRstCfg
    assertCodeString(
      id,
      """|@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT.REG init ?
         |  y.din := x.reg(1, init = ?)
         |end ID
         |""".stripMargin
    )
  }
  test("Explicit clk/rst defined") {
    class ID extends RTDesign:
      val clk = Clk      <> IN
      val rst = Rst      <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      y := x

    val id = (new ID).explicitClkRstCfg
    assertCodeString(
      id,
      """|@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class ID extends RTDesign:
         |  val clk = Clk <> IN
         |  val rst = Rst <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("RT domains get default configuration when owner is ED") {
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

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign:
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
         |    val id = ID()
         |    id.x <> x
         |  end dmn1
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val dmn2 = new RTDomain:
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
  test("Internal design generates clk and rst") {
    class ClkGen extends EDDesign:
      val src = new RTDomain:
        val clk = Clk <> IN
        val rst = Rst <> IN
      val gen = new RTDomain:
        val clk = Clk <> OUT
        val rst = Rst <> OUT
      gen.clk <> src.clk.as(gen.Clk)
      gen.rst <> src.rst.as(gen.Rst)
    class ID extends RTDesign:
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val clkGen   = new ClkGen()
      val internal = new RTDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ClkGen extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val src = new RTDomain:
         |    val clk = Clk <> IN
         |    val rst = Rst <> IN
         |  end src
         |  @timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |  @timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |  val gen = new RTDomain:
         |    val clk = Clk <> OUT
         |    val rst = Rst <> OUT
         |  end gen
         |  gen.clk <> src.clk.as(Clk)
         |  gen.rst <> src.rst.as(Rst)
         |end ClkGen
         |
         |class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val clkGen = ClkGen()
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
  test("Top-level clk/rst are VAR") {
    class FooChild extends RTDesign:
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    class Foo extends RTDesign:
      val clk = Clk <> VAR
      val rst = Rst <> VAR
      clk.actual := 0
      rst.actual := 0
      val child = new FooChild
    val top = (new Foo).explicitClkRstCfg
    assertCodeString(
      top,
      """|@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class FooChild extends RTDesign:
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |@timing.clock(rate = 50.MHz, edge = timing.clock.Edge.Rising, portName = "clk", inclusionPolicy = timing.InclusionPolicy.AsNeeded, grpName = "default")
         |@timing.reset(mode = timing.reset.Mode.Sync, active = timing.reset.Active.High, portName = "rst", inclusionPolicy = timing.InclusionPolicy.AsNeeded)
         |class Foo extends RTDesign:
         |  val clk = Clk <> VAR
         |  val rst = Rst <> VAR
         |  clk.actual := 0
         |  rst.actual := 0
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
end ExplicitClkRstCfgSpec
