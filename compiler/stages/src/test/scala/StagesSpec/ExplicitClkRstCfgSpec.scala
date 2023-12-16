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
      """|class ID extends RTDesign(main):
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
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
end ExplicitClkRstCfgSpec
