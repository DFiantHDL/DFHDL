package StagesSpec

import DFiant.*
import DFiant.compiler.stages.uniqueDesigns
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueDesignsSpec extends StageSpec:
  test("Unique designs") {
    object Container:
      class ID(using DFC) extends DFDesign:
        val x = DFSInt(15) <> IN
        val y = DFSInt(15) <> OUT
        y := x

    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      y := x

    class IDTop(using DFC) extends DFDesign:
      val x1  = DFSInt(16) <> IN
      val y1  = DFSInt(16) <> OUT
      val x2  = DFSInt(15) <> IN
      val y2  = DFSInt(15) <> OUT
      val id1 = new ID
      val id2 = new Container.ID
      id1.x <> x1
      id1.y <> y1
      id2.x <> x2
      id2.y <> y2
    val id = (new IDTop).uniqueDesigns
    assertCodeString(
      id,
      """|class ID_0(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  y := x
         |end ID_0
         |
         |class ID_1(using DFC) extends DFDesign:
         |  val x = DFSInt(15) <> IN
         |  val y = DFSInt(15) <> OUT
         |  y := x
         |end ID_1
         |
         |class IDTop(using DFC) extends DFDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val y1 = DFSInt(16) <> OUT
         |  val x2 = DFSInt(15) <> IN
         |  val y2 = DFSInt(15) <> OUT
         |  val id1 = new ID_0
         |  val id2 = new ID_1
         |  id1.x <> x1
         |  id1.y <> y1
         |  id2.x <> x2
         |  id2.y <> y2
         |end IDTop
         |""".stripMargin
    )
  }
end UniqueDesignsSpec
