package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{explicitClkRstCfg, namedAnonMultiref}
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
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Comb):
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
      ClkCfg(inclusionPolicy = ClkCfg.InclusionPolicy.AlwaysAtTop)
    given options.ElaborationOptions.DefaultRstCfg =
      RstCfg(inclusionPolicy = RstCfg.InclusionPolicy.AlwaysAtTop)
    val eo = summon[options.ElaborationOptions]
    // force DFC with these elaboration options modifications (this is required because no @top annotation)
    val dfc = DFC.empty(using eo)
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
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Default):
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
      """|class ID extends RTDesign(RTDomainCfg.Default):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = sd"16'0")
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Default):
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
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(RTDomainCfg.Comb):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(RTDomainCfg.Comb):
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y
         |  y <> id.y
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
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(RTDomainCfg.Default):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(RTDomainCfg.Comb):
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y.reg(1, init = sd"16'0")
         |  y <> id.y
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
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RelatedDomain:
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y
         |  y <> id.y
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
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain:
        val id = ID()
        id.x <> dmn1.id.y
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Default):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RelatedDomain:
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y.reg(1, init = sd"16'0")
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Basic hierarchy with domains registered, top domain = RT, custom config") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    val cfg2   = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x.reg(1, init = 5)

    class IDTop extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain(cfg):
        val id = ID()
        id.x <> x
      val dmn2 = new RTDomain(cfg2):
        val id = ID()
        id.x <> dmn1.id.y
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).explicitClkRstCfg
    // TODO: figure out why no different ID designs with different configurations are created
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = sd"16'5")
         |end ID
         |
         |class IDTop extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(cfg):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(cfg2):
         |    val id = ID()
         |    id.x <> dmn1.id.y
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y.reg(1, init = sd"16'0")
         |  y <> id.y
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

    val top = (new TrueDPR()).namedAnonMultiref.explicitClkRstCfg
    assertCodeString(
      top,
      """|class TrueDPR(
         |    val DATA_WIDTH: Int <> CONST = 8,
         |    val ADDR_WIDTH: Int <> CONST = 8
         |) extends EDDesign:
         |  val RAM_LENGTH: Int <> CONST = 2 ** ADDR_WIDTH
         |  val ram = Bits(DATA_WIDTH) X RAM_LENGTH <> VAR.SHARED
         |  val a = new RTDomain(RTDomainCfg.Default.norst):
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT.REG
         |    val we = Bit <> IN
         |    if (we) ram(addr.uint.toInt) := data
         |    q.din := ram(addr.uint.toInt)
         |  val b = new RTDomain(RTDomainCfg.Default.norst):
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT.REG
         |    val we = Bit <> IN
         |    if (we) ram(addr.uint.toInt) := data
         |    q.din := ram(addr.uint.toInt)
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
      """|class ID extends RTDesign(RTDomainCfg.Default.norst):
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
      """|class ID extends RTDesign(RTDomainCfg.Default):
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

    val id = (new IDTop).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(RTDomainCfg.Default):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(RTDomainCfg.Default):
         |    val id = ID()
         |    id.x <> dmn1.id.y.reg(1, init = sd"16'0")
         |  val dmn3 = new dmn1.RelatedDomain:
         |    val id = ID()
         |    id.x <> dmn2.id.y.reg(1, init = sd"16'0")
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Internal design generates clk and rst") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    val genCfg = RTDomainCfg(clkCfg, rstCfg)
    class ClkGen(srcCfg: RTDomainCfg, genCfg: RTDomainCfg) extends EDDesign:
      val src = new RTDomain(srcCfg):
        val clk = Clk <> IN
        val rst = Rst <> IN
      val gen = new RTDomain(genCfg):
        val clk = Clk <> OUT
        val rst = Rst <> OUT
      gen.clk <> src.clk
      gen.rst <> src.rst
    class ID extends RTDesign(cfg):
      val x      = SInt(16) <> IN
      val y      = SInt(16) <> OUT
      val clkGen = new ClkGen(cfg, genCfg)
      val internal = new RTDomain(genCfg):
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).explicitClkRstCfg
    assertCodeString(
      id,
      """|class ClkGen extends EDDesign:
         |  val src = new RTDomain(cfg):
         |    val clk = Clk <> IN
         |    val rst = Rst <> IN
         |  val gen = new RTDomain(genCfg):
         |    val clk = Clk <> OUT
         |    val rst = Rst <> OUT
         |  gen.clk <> src.clk
         |  gen.rst <> src.rst
         |end ClkGen
         |
         |class ID extends RTDesign(cfg):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val clkGen = ClkGen()
         |  val internal = new RTDomain(genCfg):
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end ExplicitClkRstCfgSpec
