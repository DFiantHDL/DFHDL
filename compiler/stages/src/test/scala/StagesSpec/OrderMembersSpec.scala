package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.simpleOrder
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class OrderMembersSpec extends StageSpec:
  test("Simple order") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val z = Bits(8)  <> VAR
      val y = SInt(16) <> OUT
      y := x
    val id = (new ID).simpleOrder
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = Bits(8) <> VAR
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end OrderMembersSpec
