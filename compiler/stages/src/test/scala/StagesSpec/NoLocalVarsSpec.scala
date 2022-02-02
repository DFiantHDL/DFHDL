package StagesSpec

import DFiant.*
import DFiant.compiler.stages.noLocalVars
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NoLocalVarsSpec extends StageSpec:
  test("Nested local var move") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z = DFSInt(16) <> VAR
      z := x
      if (x > 5)
        val zz = DFSInt(16) <> VAR
        x match
          case 2 =>
            val zzz = DFSInt(16) <> VAR init 0
            zzz := zzz.prev(1) + 1
          case _ =>
        zz := x
        z  := zz
      y := z
    end ID
    val id = (new ID).noLocalVars
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = DFSInt(16) <> VAR
         |  z := x
         |  val zz = DFSInt(16) <> VAR
         |  val zzz = DFSInt(16) <> VAR init sd"16'0"
         |  if (x > d"16'5")
         |    x match
         |      case sd"16'2" => zzz := zzz.prev + sd"2'1"
         |      case _ =>
         |    zz := x
         |    z := zz
         |  y := z
         |end ID
         |""".stripMargin
    )
  }
end NoLocalVarsSpec
