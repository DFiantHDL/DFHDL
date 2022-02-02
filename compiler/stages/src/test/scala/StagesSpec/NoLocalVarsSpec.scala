package StagesSpec

import DFiant.*
import DFiant.compiler.stages.noLocalVars
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NoLocalVarsSpec extends StageSpec:
  test("Basic local var move") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z = DFSInt(16) <> VAR
      z := x
      if (x > 5)
        val zz = DFSInt(16) <> VAR
        zz := x
        z  := zz
      y := z
    val id = (new ID).noLocalVars
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = DFSInt(16) <> VAR
         |  z := x
         |  val zz = DFSInt(16) <> VAR
         |  if (x > d"16'5")
         |    zz := x
         |    z := zz
         |  y := z
         |end ID
         |""".stripMargin
    )
  }
end NoLocalVarsSpec
