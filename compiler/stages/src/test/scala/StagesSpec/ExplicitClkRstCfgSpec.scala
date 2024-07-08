package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitClkRstCfg
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitClkRstCfgSpec extends StageSpec:
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
      """|class ID extends RTDesign(comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(comb):
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
      """|class ID extends RTDesign(main):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x.reg(1, init = sd"16'0")
         |end ID
         |
         |class IDTop extends RTDesign(main):
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
      """|class ID extends RTDesign(comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(comb):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(comb):
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
      """|class ID extends RTDesign(comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain(main):
         |    val id = ID()
         |    id.x <> x
         |  val dmn2 = new RTDomain(comb):
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
      """|class ID extends RTDesign(comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(comb):
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
      """|class ID extends RTDesign(comb):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends RTDesign(main):
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
         |class IDTop extends RTDesign(comb):
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
end ExplicitClkRstCfgSpec
