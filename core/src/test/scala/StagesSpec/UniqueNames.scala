package StagesSpec

import DFiant.*
import DFiant.compiler.stages.uniqueNames
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueNames extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    val z = DFBits(8)  <> IN
    y := x
    object Temp:
      val x = DFBit  <> VAR init 1
      val Y = DFBool <> VAR init 0
    Temp.x // touch to force evaluation

  test("Unique names case-sensitive") {
    val id = (new ID).uniqueNames(Set("z"), true)
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x_0 = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z_0 = DFBits(8) <> IN
         |  y := x_0
         |  val x_1 = DFBit <> VAR init 1
         |  val Y = DFBool <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  test("Unique names case-insensitive") {
    val id = (new ID).uniqueNames(Set(), false)
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x_0 = DFSInt(16) <> IN
         |  val y_0 = DFSInt(16) <> OUT
         |  val z = DFBits(8) <> IN
         |  y_0 := x_0
         |  val x_1 = DFBit <> VAR init 1
         |  val Y_1 = DFBool <> VAR init false
         |end ID
         |""".stripMargin
    )
  }

end UniqueNames
