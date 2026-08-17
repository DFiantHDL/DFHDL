package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{dropDomains, getCodeString}
import dfhdl.hw.annotation.flattenMode
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
class DropDomainsSpec extends StageSpec:
  test("Mixed domain composition") {
    class IDTop extends EDDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val dmn1 = new EDDomain:
        val dmn2 = new EDDomain:
          val v = SInt(16) <> VAR init 0
          y <> x + v
      @flattenMode.transparent
      val dmn3 = new EDDomain:
        @flattenMode.prefix("")
        val dmn4 = new EDDomain:
          val x = SInt(16) <> IN
          val y = SInt(16) <> OUT
          y <> x

      @flattenMode.transparent
      val dmn5 = new EDDomain:
        @flattenMode.transparent
        val dmn6 = new EDDomain:
          val x = SInt(16) <> IN
          val y = SInt(16) <> OUT
          y <> x
    end IDTop

    val top = (new IDTop).dropDomains.getCodeString
    assertNoDiff(
      top,
      """|class IDTop extends EDDesign:
         |  val x_0 = SInt(16) <> IN
         |  val y_0 = SInt(16) <> OUT
         |  val dmn1_dmn2_v = SInt(16) <> VAR init sd"16'0"
         |  y_0 <> (x_0 + dmn1_dmn2_v)
         |  val dmn4x = SInt(16) <> IN
         |  val dmn4y = SInt(16) <> OUT
         |  dmn4y <> dmn4x
         |  val x_1 = SInt(16) <> IN
         |  val y_1 = SInt(16) <> OUT
         |  y_1 <> x_1
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

    val top = (new IDTop).dropDomains
    assertCodeString(
      top,
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
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val dmn1_clk = Clk_default <> IN
         |  val dmn1_rst = Rst_default <> IN
         |  val dmn1_id = ID()
         |  dmn1_id.x <> x
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val dmn2_clk = Clk_default <> IN
         |  val dmn2_rst = Rst_default <> IN
         |  val dmn2_dmn1_id_y_reg = SInt(16) <> VAR
         |  val dmn2_id = ID()
         |  dmn2_id.x <> dmn2_dmn1_id_y_reg
         |  process(dmn2_clk):
         |    if (dmn2_clk.actual.rising)
         |      if (dmn2_rst.actual == 1) dmn2_dmn1_id_y_reg :== sd"16'0"
         |      else dmn2_dmn1_id_y_reg :== dmn1_id.y
         |    end if
         |  val dmn3_dmn2_id_y_reg = SInt(16) <> VAR
         |  val dmn3_id = ID()
         |  dmn3_id.x <> dmn3_dmn2_id_y_reg
         |  process(dmn1_clk):
         |    if (dmn1_clk.actual.rising)
         |      if (dmn1_rst.actual == 1) dmn3_dmn2_id_y_reg :== sd"16'0"
         |      else dmn3_dmn2_id_y_reg :== dmn2_id.y
         |    end if
         |  y <> dmn3_id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("By-name selection of a domain-nested derived clock follows the flattened name") {
    class Leaf extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0
      y.din := x
      @hw.constraints.timing.related(this)
      val active = new RTDomain:
        val clk = Clk <> IN
        val z   = SInt(16) <> OUT.REG init 0
        z.din := x
      @hw.constraints.timing.related(this)
      @flattenMode.suffix("_")
      val slow = new RTDomain:
        val clk = Clk <> IN
        val w   = SInt(16) <> OUT.REG init 0
        w.din := x
    class Top extends RTDesign:
      val x     = SInt(16) <> IN
      val y     = SInt(16) <> OUT
      val gclk1 = Bit      <> IN
      val gclk2 = Bit      <> IN
      val leaf  = Leaf()
      leaf.x <> x
      y      <> leaf.y
      leaf.active.clk <> gclk1.as(leaf.active.Clk)
      leaf.slow.clk   <> gclk2.as(leaf.slow.Clk)
    val top = (new Top).dropDomains
    assertCodeString(
      top,
      """|case class Clk_default() extends Clk
         |case class Rst_default() extends Rst
         |case class Clk_active_clk() extends Clk
         |case class Clk_slow_clk() extends Clk
         |
         |class Leaf extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val active_clk = Clk_active_clk <> IN
         |  val active_z = SInt(16) <> OUT
         |  process(active_clk):
         |    if (active_clk.actual.rising)
         |      if (rst.actual == 1) active_z :== sd"16'0"
         |      else active_z :== x
         |    end if
         |  val clk_slow = Clk_slow_clk <> IN
         |  val w_slow = SInt(16) <> OUT
         |  process(clk_slow):
         |    if (clk_slow.actual.rising)
         |      if (rst.actual == 1) w_slow :== sd"16'0"
         |      else w_slow :== x
         |    end if
         |  process(clk):
         |    if (clk.actual.rising)
         |      if (rst.actual == 1) y :== sd"16'0"
         |      else y :== x
         |    end if
         |end Leaf
         |
         |class Top extends EDDesign:
         |  @timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
         |  val clk = Clk_default <> IN
         |  val rst = Rst_default <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val gclk1 = Bit <> IN
         |  val gclk2 = Bit <> IN
         |  val leaf = Leaf()
         |  leaf.x <> x
         |  y <> leaf.y
         |  leaf.active_clk <> gclk1.as(Clk_active_clk)
         |  leaf.clk_slow <> gclk2.as(Clk_slow_clk)
         |end Top
         |""".stripMargin
    )
  }
end DropDomainsSpec
