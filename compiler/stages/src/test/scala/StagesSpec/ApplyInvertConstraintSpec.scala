package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.applyInvertConstraint
import dfhdl.hw.constraints.io
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ApplyInvertConstraintSpec extends StageSpec:
  test("Basic input port inversion") {
    class ID extends RTDesign:
      @io(loc = "xloc", invertActiveState = true)
      val x = Bit <> IN
      val y = Bit <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(loc = "xloc")
         |  val x = Bit <> IN
         |  val x_inverted = Bit <> VAR
         |  x_inverted <> (!x)
         |  val y = Bit <> OUT
         |  y <> x_inverted
         |end ID
         |""".stripMargin
    )
  }

  test("Basic output port inversion") {
    class ID extends RTDesign:
      val x = Bit <> IN
      @io(loc = "yloc", invertActiveState = true)
      val y = Bit <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = Bit <> IN
         |  @io(loc = "yloc")
         |  val y = Bit <> OUT
         |  val y_inverted = Bit <> VAR
         |  y <> (!y_inverted)
         |  y_inverted <> x
         |end ID
         |""".stripMargin
    )
  }

  test("Both input and output ports inverted") {
    class ID extends RTDesign:
      @io(loc = "xloc", invertActiveState = true)
      val x = Bit <> IN
      @io(loc = "yloc", invertActiveState = true)
      val y = Bit <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(loc = "xloc")
         |  val x = Bit <> IN
         |  val x_inverted = Bit <> VAR
         |  x_inverted <> (!x)
         |  @io(loc = "yloc")
         |  val y = Bit <> OUT
         |  val y_inverted = Bit <> VAR
         |  y <> (!y_inverted)
         |  y_inverted <> x_inverted
         |end ID
         |""".stripMargin
    )
  }

  test("Bits type inversion using bitwise NOT") {
    class ID extends RTDesign:
      @io(bitIdx = 0, loc = "xloc0", invertActiveState = true)
      @io(bitIdx = 1, loc = "xloc1", invertActiveState = true)
      @io(bitIdx = 2, loc = "xloc2", invertActiveState = true)
      @io(bitIdx = 3, loc = "xloc3", invertActiveState = true)
      val x = Bits(4) <> IN
      val y = Bits(4) <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(bitIdx = 0, loc = "xloc0")
         |  @io(bitIdx = 1, loc = "xloc1")
         |  @io(bitIdx = 2, loc = "xloc2")
         |  @io(bitIdx = 3, loc = "xloc3")
         |  val x = Bits(4) <> IN
         |  val x_inverted = Bits(4) <> VAR
         |  x_inverted <> (~x)
         |  val y = Bits(4) <> OUT
         |  y <> x_inverted
         |end ID
         |""".stripMargin
    )
  }

  test("Bits type inversion using bitwise NOT with mask") {
    class ID extends RTDesign:
      @io(bitIdx = 0, loc = "xloc0", invertActiveState = true)
      @io(bitIdx = 1, loc = "xloc1")
      @io(bitIdx = 2, loc = "xloc2")
      @io(bitIdx = 3, loc = "xloc3", invertActiveState = true)
      val x = Bits(4) <> IN
      val y = Bits(4) <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(bitIdx = 0, loc = "xloc0")
         |  @io(bitIdx = 1, loc = "xloc1")
         |  @io(bitIdx = 2, loc = "xloc2")
         |  @io(bitIdx = 3, loc = "xloc3")
         |  val x = Bits(4) <> IN
         |  val x_inverted = Bits(4) <> VAR
         |  x_inverted <> (x ^ h"9")
         |  val y = Bits(4) <> OUT
         |  y <> x_inverted
         |end ID
         |""".stripMargin
    )
  }

  test("Multiple ports with mixed inversion") {
    class ID extends RTDesign:
      @io(loc = "xloc1", invertActiveState = true)
      val x1 = Bit <> IN
      val y1 = Bit <> OUT
      @io(bitIdx = 0, loc = "xloc0", invertActiveState = false)
      @io(bitIdx = 1, loc = "xloc1", invertActiveState = true)
      @io(bitIdx = 2, loc = "xloc2", invertActiveState = true)
      @io(bitIdx = 3, loc = "xloc3", invertActiveState = true)
      val x2 = Bits(4) <> IN
      val y2 = Bits(4) <> OUT
      y1 <> x1
      y2 <> x2
    val id = (new ID).applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(loc = "xloc1")
         |  val x1 = Bit <> IN
         |  val x1_inverted = Bit <> VAR
         |  x1_inverted <> (!x1)
         |  val y1 = Bit <> OUT
         |  @io(bitIdx = 0, loc = "xloc0")
         |  @io(bitIdx = 1, loc = "xloc1")
         |  @io(bitIdx = 2, loc = "xloc2")
         |  @io(bitIdx = 3, loc = "xloc3")
         |  val x2 = Bits(4) <> IN
         |  val x2_inverted = Bits(4) <> VAR
         |  x2_inverted <> (x2 ^ h"e")
         |  val y2 = Bits(4) <> OUT
         |  y1 <> x1_inverted
         |  y2 <> x2_inverted
         |end ID
         |""".stripMargin
    )
  }

  test("Double application has no effect") {
    class ID extends RTDesign:
      @io(loc = "xloc", invertActiveState = true)
      val x = Bit <> IN
      val y = Bit <> OUT
      y <> x
    val id = (new ID).applyInvertConstraint.applyInvertConstraint
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  @io(loc = "xloc")
         |  val x = Bit <> IN
         |  val x_inverted = Bit <> VAR
         |  x_inverted <> (!x)
         |  val y = Bit <> OUT
         |  y <> x_inverted
         |end ID
         |""".stripMargin
    )
  }
end ApplyInvertConstraintSpec
