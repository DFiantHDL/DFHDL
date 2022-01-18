package StagesSpec

import DFiant.*
import DFiant.compiler.stages.getCodeString
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintCodeStringSpec extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    y := x

  class IDTop(using DFC) extends DFDesign:
    val x   = DFSInt(16) <> IN
    val y   = DFSInt(16) <> OUT
    val id1 = new ID
    val id2 = new ID
    id1.x <> x
    id1.y <> id2.x
    id2.y <> y

  class IDTopVia(using DFC) extends DFDesign:
    self =>
    val x     = DFSInt(16) <> IN
    val y     = DFSInt(16) <> OUT
    val id1_x = DFSInt(16) <> IN
    val id1_y = DFSInt(16) <> OUT
    val id2_x = DFSInt(16) <> IN
    val id2_y = DFSInt(16) <> OUT
    val id1 = new ID:
      this.x <> id1_x
      this.y <> id1_y
    val id2 = new ID:
      this.x <> id2_x
      this.y <> id2_y
    x     <> id1_x
    id1_y <> id2_x
    y     <> id2_y
  end IDTopVia

  test("Basic ID design") {
    val id = (new ID).getCodeString
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
  test("Basic ID design hierarchy") {
    val id = (new IDTop).getCodeString
    assertNoDiff(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val id1 = new ID
         |  val id2 = new ID
         |  id1.x <> x
         |  id1.y <> id2.x
         |  id2.y <> y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Via-connection ID design hierarchy") {
    val id = (new IDTopVia).getCodeString
    assertNoDiff(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTopVia(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val id1_x = DFSInt(16) <> IN
         |  val id1_y = DFSInt(16) <> OUT
         |  val id2_x = DFSInt(16) <> IN
         |  val id2_y = DFSInt(16) <> OUT
         |  val id1 = new ID:
         |    this.x <> id1_x
         |    this.y <> id1_y
         |  val id2 = new ID:
         |    this.x <> id2_x
         |    this.y <> id2_y
         |  x <> id1_x
         |  id1_y <> id2_x
         |  y <> id2_y
         |end IDTopVia
         |""".stripMargin
    )
  }
end PrintCodeStringSpec
