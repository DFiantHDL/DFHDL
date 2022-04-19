package StagesSpec

import DFiant.*
import DFiant.compiler.stages.addClkRst
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class AddClkRstSpec extends StageSpec:
  val clkCfg      = ClkCfg(ClkCfg.Edge.Rising)
  val rstCfg      = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
  val cfg         = RTDomainCfg(clkCfg, rstCfg)
  val cfgI        = RTDomainCfg(clkCfg, rstCfg)
  val cfgNoRst    = RTDomainCfg(clkCfg, None)
  val cfgNoRstI   = RTDomainCfg(clkCfg, None)
  val cfgNoClkRst = RTDomainCfg(None, None)
  test("Basic design clk and rst addition") {
    class ID extends RTDesign(cfg):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal = new RTDomain(cfgI):
        val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfgI):
         |    val clk = DFBit <> IN
         |    val rst = DFBit <> IN
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Clk and rst already exist") {
    class ID extends RTDesign(cfg):
      val clk = DFBit      <> IN
      val rst = DFBit      <> IN
      val x   = DFSInt(16) <> IN
      val y   = DFSInt(16) <> OUT
      val internal = new RTDomain(cfgI):
        val clk = DFBit      <> IN
        val rst = DFBit      <> IN
        val x   = DFSInt(16) <> IN
        val y   = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfgI):
         |    val clk = DFBit <> IN
         |    val rst = DFBit <> IN
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No rst") {
    class ID extends RTDesign(cfgNoRst):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal = new RTDomain(cfgNoRstI):
        val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfgNoRst):
         |  val clk = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfgNoRstI):
         |    val clk = DFBit <> IN
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("No clk and rst") {
    class ID extends RTDesign(cfgNoClkRst):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal = new RTDomain(cfgNoRstI):
        val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfgNoClkRst):
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfgNoRstI):
         |    val clk = DFBit <> IN
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between design and internal domain") {
    class ID extends RTDesign(cfg):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal = new RTDomain(cfg):
        val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfg):
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Add once for the same domain config between internal domains") {
    class ID extends RTDesign(cfg):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal1 = new RTDomain(cfgI):
        val ii = new RTDomain(cfgI):
          val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        x <> y
      val internal2 = new RTDomain(cfgI):
        val x = DFSInt(16) <> IN
        val y = DFSInt(16) <> OUT
        y <> internal1.ii.x + x
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal1 = new RTDomain(cfgI):
         |    val clk = DFBit <> IN
         |    val rst = DFBit <> IN
         |    val ii = new RTDomain(cfgI):
         |      val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  val internal2 = new RTDomain(cfgI):
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> internal1.ii.x + x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Explicit clk and rst are kept") {
    class ID extends RTDesign(cfg):
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val internal = new RTDomain(cfg):
        val clk = DFBit      <> IN
        val rst = DFBit      <> IN
        val x   = DFSInt(16) <> IN
        val y   = DFSInt(16) <> OUT
        x <> y
      y := x
    val id = (new ID).addClkRst
    assertCodeString(
      id,
      """|class ID extends RTDesign(cfg):
         |  val clk = DFBit <> IN
         |  val rst = DFBit <> IN
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val internal = new RTDomain(cfg):
         |    val clk = DFBit <> IN
         |    val rst = DFBit <> IN
         |    val x = DFSInt(16) <> IN
         |    val y = DFSInt(16) <> OUT
         |    y <> x
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end AddClkRstSpec
