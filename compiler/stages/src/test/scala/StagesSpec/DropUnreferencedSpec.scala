package StagesSpec

import dfhdl.*
import compiler.stages.dropUnreferencedVars
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropUnreferencedSpec extends StageSpec:
  test("Drop unreferenced") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = Bits(8)  <> VAR
      y := x
    val id = (new ID).dropUnreferencedVars
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Keep initialized unreferenced") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = Bits(8)  <> VAR init all(0)
      y := x
    val id = (new ID).dropUnreferencedVars
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = Bits(8) <> VAR init h"00"
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end DropUnreferencedSpec
