package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.moveBlockingAssignmentFromSeqProc
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class MoveBlockingAssignmentFromSeqProcSpec extends StageSpec:
  test("moving sequential process blocking assignments"):
    class ID extends EDDesign:
      val clk = Bit      <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val z   = SInt(16) <> VAR
      val z2  = SInt(16) <> VAR
      process(clk.rising):
        z := x + 1
        y :== z
        if (x > 0)
          z2 := x + 1
          y :== z2

    val id = (new ID).moveBlockingAssignmentFromSeqProc
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  z <> x + sd"2'1"
         |  process(clk.rising):
         |    y :== z
         |end ID
         |""".stripMargin
    )

  test("keeping a combinational process blocking assignments"):
    class ID extends EDDesign:
      val clk = Bit      <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val z   = SInt(16) <> VAR
      process(all):
        z := x + 1
        y :== z

    val id = (new ID).moveBlockingAssignmentFromSeqProc
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  process(all):
         |    z := x + sd"2'1"
         |    y :== z
         |end ID
         |""".stripMargin
    )
end MoveBlockingAssignmentFromSeqProcSpec
