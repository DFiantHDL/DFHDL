package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.addClkRst
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class AddClkRstSpec extends StageSpec:
  val clkCfg    = ClkCfg(ClkCfg.Edge.Rising)
  val rstCfg    = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
  val cfg       = RTDomainCfg(clkCfg, rstCfg)
  val cfgI      = RTDomainCfg(clkCfg, rstCfg)
  val cfgNoRst  = RTDomainCfg(clkCfg, None)
  val cfgNoRstI = RTDomainCfg(clkCfg, None)
  test("Basic design clk and rst addition") {
    class ID extends RTDesign(cfg):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfgI):
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
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfgI):
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic hierarchy, applied twice") {
    class ID extends RTDesign(cfg):
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x.reg(1, init = ?)

    class IDTop extends RTDesign(cfg):
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
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = ?)
         |end ID
         |
         |class IDTop extends RTDesign(cfg):
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
    class ID extends RTDesign(cfg):
      val clk      = Clk      <> IN
      val rst      = Rst      <> IN
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfgI):
        val clk = Clk      <> IN
        val rst = Rst      <> IN
        val x   = SInt(16) <> IN
        val y   = SInt(16) <> OUT
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
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfgI):
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No rst") {
    class ID extends RTDesign(cfgNoRst):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfgNoRstI):
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
         |class ID extends RTDesign(cfgNoRst):
         |  val clk = Clk_cfgNoRst <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfgNoRstI):
         |    val clk = Clk_cfgNoRstI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No clk and rst") {
    class ID extends RTDesign(RTDomainCfg.Comb):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfgNoRstI):
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfgNoRstI() extends Clk
         |
         |class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfgNoRstI):
         |    val clk = Clk_cfgNoRstI <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between design and internal related domain") {
    class ID extends RTDesign(cfg):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RelatedDomain:
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
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RelatedDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between internal related domains") {
    class ID extends RTDesign(cfg):
      val x         = SInt(16) <> IN
      val y         = SInt(16) <> OUT
      val internal1 = new RTDomain(cfgI):
        val ii = new RelatedDomain:
          val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        x <> y
      val internal2 = new internal1.RelatedDomain:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT
        y <> internal1.ii.x + x
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |case class Clk_cfgI() extends Clk
         |case class Rst_cfgI() extends Rst
         |
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal1 = new RTDomain(cfgI):
         |    val clk = Clk_cfgI <> IN
         |    val rst = Rst_cfgI <> IN
         |    val ii = new RelatedDomain:
         |      val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  val internal2 = new internal1.RelatedDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> (internal1.ii.x + x)
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Explicit clk and rst are kept") {
    class ID extends RTDesign(cfg):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfg):
        val clk = Clk      <> IN
        val rst = Rst      <> IN
        val x   = SInt(16) <> IN
        val y   = SInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfg):
         |    val clk = Clk_cfg <> IN
         |    val rst = Rst_cfg <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Internal design generates clk and rst") {
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
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val clkGen   = new ClkGen(cfg, genCfg)
      val internal = new RTDomain(genCfg):
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
         |  val src = new RTDomain(cfg):
         |    val clk = Clk_cfg <> IN
         |    val rst = Rst_cfg <> IN
         |  val gen = new RTDomain(genCfg):
         |    val clk = Clk_genCfg <> OUT
         |    val rst = Rst_genCfg <> OUT
         |  gen.clk <> src.clk
         |  gen.rst <> src.rst
         |end ClkGen
         |
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
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
  test("Basic hierarchy with domains") {
    class ID extends RTDesign(RTDomainCfg.Comb):
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
      val dmn3 = new dmn1.RelatedDomain:
        val id = ID()
        id.x <> dmn2.id.y.reg(1, init = 0)
      y <> dmn3.id.y

    val id = (new IDTop).addClkRst
    assertCodeString(
      id,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |
         |class ID extends RTDesign(RTDomainCfg.Comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(RTDomainCfg.Default):
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(RTDomainCfg.Default):
         |    val clk = Clk_default <> IN
         |    val rst = Rst_default <> IN
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
  test("Derive config with `.norst`") {
    class ID extends RTDesign(cfg):
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = new RTDomain(cfg.norst):
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
         |class ID extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = new RTDomain(cfg.norst):
         |    val clk = Clk_cfg <> IN
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Top-level clk/rst are VARs") {
    class FooChild extends RTDesign:
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    class Foo extends RTDesign(cfg):
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
         |class FooChild extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |class Foo extends RTDesign(cfg):
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

    class Foo extends RTDesign(cfg):
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class FooChild extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |class Foo extends RTDesign(cfg):
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

    class Foo extends RTDesign(cfg):
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfg() extends Clk
         |case class Rst_cfg() extends Rst
         |
         |class FooChild extends RTDesign(cfg):
         |  val clk = Clk_cfg <> IN
         |  val rst = Rst_cfg <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |class Foo extends RTDesign(cfg):
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
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation clk only generated") {
    class FooChild extends RTDesign(cfgNoRst):
      val y = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    class Foo extends RTDesign(cfgNoRst):
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfgNoRst() extends Clk
         |
         |class FooChild extends RTDesign(cfgNoRst):
         |  val clk = Clk_cfgNoRst <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |class Foo extends RTDesign(cfgNoRst):
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
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
  test("Top-level simulation internal clk declared") {
    class FooChild extends RTDesign(cfgNoRst):
      val clk = Clk     <> IN
      val y   = UInt(8) <> OUT.REG init 0
      y.din := y + 1

    class Foo extends RTDesign(cfgNoRst):
      val child = new FooChild
    val top = (new Foo).addClkRst
    assertCodeString(
      top,
      """|case class Clk_cfgNoRst() extends Clk
         |
         |class FooChild extends RTDesign(cfgNoRst):
         |  val clk = Clk_cfgNoRst <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  y.din := y + d"8'1"
         |end FooChild
         |
         |class Foo extends RTDesign(cfgNoRst):
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
         |  val child = FooChild()
         |end Foo
         |""".stripMargin
    )
  }
end AddClkRstSpec
