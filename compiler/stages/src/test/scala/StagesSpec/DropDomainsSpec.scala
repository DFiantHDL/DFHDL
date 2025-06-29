package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{dropDomains, getCodeString}
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
      @hw.flattenMode.transparent
      val dmn3 = new EDDomain:
        @hw.flattenMode.prefix("")
        val dmn4 = new EDDomain:
          val x = SInt(16) <> IN
          val y = SInt(16) <> OUT
          y <> x

      @hw.flattenMode.transparent
      val dmn5 = new EDDomain:
        @hw.flattenMode.transparent
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
      val dmn3 = new dmn1.RelatedDomain:
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
         |  val dmn1_clk = Clk_default <> IN
         |  val dmn1_rst = Rst_default <> IN
         |  val dmn1_id = ID()
         |  dmn1_id.x <> x
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
end DropDomainsSpec
