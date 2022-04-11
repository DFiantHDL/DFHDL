package StagesSpec

import DFiant.*
import DFiant.compiler.stages.simpleOrder
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class OrderMembersSpec extends StageSpec:
  test("Simple order") {
    class ID extends DFDesign:
      val x = DFSInt(16) <> IN
      val z = DFBits(8)  <> VAR
      val y = DFSInt(16) <> OUT
      y := x
    val id = (new ID).simpleOrder
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = DFBits(8) <> VAR
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end OrderMembersSpec
