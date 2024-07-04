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
  test("Basic hierarchy with domains") {
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
      """|class ID extends RTDesign(main):
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
         |  val dmn2 = new RTDomain(main):
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
end ExplicitClkRstCfgSpec
