package StagesSpec

import DFiant.*
import compiler.stages.dropUnreferenced
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropUnreferencedSpec extends StageSpec:
  test("Drop unreferenced") {
    class ID extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z = Bits(8)    <> VAR
      y := x
    val id = (new ID).dropUnreferenced
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Keep initialized unreferenced") {
    class ID extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z = Bits(8)    <> VAR init all(0)
      y := x
    val id = (new ID).dropUnreferenced
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = Bits(8) <> VAR init h"00"
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end DropUnreferencedSpec
