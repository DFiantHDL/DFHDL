package StagesSpec

import DFiant.*
import DFiant.compiler.stages.explicitPrev
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitPrevSpec extends StageSpec:
  test("Basic explicit prev") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT init 0
      y := y + 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT init sd"16'0"
         |  y := y.prev
         |  y := y + sd"2'1"
         |end ID
         |""".stripMargin
    )
  }
//  test("Drop unreferenced") {
//    class ID(using DFC) extends DFDesign:
//      val x = DFSInt(16) <> IN
//      val y = DFSInt(16) <> OUT
//      val z = DFBits(8)  <> VAR
//      y := z
//      y := x
//    val id = (new ID).explicitPrev
//    assertCodeString(
//      id,
//      """|class ID(using DFC) extends DFDesign:
//         |  val x = DFSInt(16) <> IN
//         |  val y = DFSInt(16) <> OUT
//         |  y := x
//         |end ID
//         |""".stripMargin
//    )
//  }
end ExplicitPrevSpec
