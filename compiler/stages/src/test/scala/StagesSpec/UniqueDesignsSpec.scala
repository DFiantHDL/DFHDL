package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.uniqueDesigns
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueDesignsSpec extends StageSpec:
  test("Unique designs") {
    object Container:
      class ID extends DFDesign:
        val x = SInt(15) <> IN
        val y = SInt(15) <> OUT
        y := x

    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends DFDesign:
      val x1  = SInt(16) <> IN
      val y1  = SInt(16) <> OUT
      val x2  = SInt(15) <> IN
      val y2  = SInt(15) <> OUT
      val id1 = new ID
      val id2 = new Container.ID
      id1.x <> x1
      id1.y <> y1
      id2.x <> x2
      id2.y <> y2
    val id = (new IDTop).uniqueDesigns
    assertCodeString(
      id,
      """|class ID_0 extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID_0
         |
         |class ID_1 extends DFDesign:
         |  val x = SInt(15) <> IN
         |  val y = SInt(15) <> OUT
         |  y := x
         |end ID_1
         |
         |class IDTop extends DFDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(15) <> IN
         |  val y2 = SInt(15) <> OUT
         |  val id1 = ID_0()
         |  val id2 = ID_1()
         |  id1.x <> x1
         |  y1 <> id1.y
         |  id2.x <> x2
         |  y2 <> id2.y
         |end IDTop
         |""".stripMargin
    )
  }
end UniqueDesignsSpec
