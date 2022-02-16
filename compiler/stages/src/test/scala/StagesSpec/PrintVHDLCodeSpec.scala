package StagesSpec

import DFiant.*
import DFiant.compiler.stages.vhdl.getVHDLCode
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintVHDLCodeSpec extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    y := x

  test("Basic ID design") {
    val id = (new ID).getVHDLCode
    assertNoDiff(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end PrintVHDLCodeSpec
