package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.uniqueDesigns
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueDesignsSpec extends StageSpec:
  test("Reduplicated identical designs are re-shared into one") {
    // `ReduplicateDesign` clones the whole `dup_*` sub-tree for ref uniqueness,
    // producing two structurally-identical `Leaf` designs. `UniqueDesigns` must
    // re-share them into a SINGLE `Leaf` design referenced by both instances.
    class Leaf extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := x
    class Inner extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      val l = Leaf()
      l.x <> x
      l.y <> y
    class Top extends DFDesign:
      val in_a  = UInt(8) <> IN
      val out_a = UInt(8) <> OUT
      val in_b  = UInt(8) <> IN
      val out_b = UInt(8) <> OUT
      val dup_a = Inner()
      val dup_b = Inner()
      dup_a.x <> in_a
      dup_a.y <> out_a
      dup_b.x <> in_b
      dup_b.y <> out_b
    val id = (new Top).reduplicateNamedDup.uniqueDesigns
    assertCodeString(
      id,
      """|class Leaf extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Leaf
         |
         |class Inner_dup_a extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val l = Leaf()
         |  l.x <> x
         |  y <> l.y
         |end Inner_dup_a
         |
         |class Inner_dup_b extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val l = Leaf()
         |  l.x <> x
         |  y <> l.y
         |end Inner_dup_b
         |
         |class Top extends DFDesign:
         |  val in_a = UInt(8) <> IN
         |  val out_a = UInt(8) <> OUT
         |  val in_b = UInt(8) <> IN
         |  val out_b = UInt(8) <> OUT
         |  val dup_a = Inner_dup_a()
         |  val dup_b = Inner_dup_b()
         |  dup_a.x <> in_a
         |  out_a <> dup_a.y
         |  dup_b.x <> in_b
         |  out_b <> dup_b.y
         |end Top
         |""".stripMargin
    )
  }
  test("Identical parameterized instances with computed sub-param should share") {
    class Leaf(val W: Int <> CONST = 8, val F: Int <> CONST = 4) extends EDDesign:
      val x = SInt(W) <> IN
      val y = SInt(W) <> OUT
      process(all):
        y := x

    class Mid(val FP_WIDTH: Int <> CONST = 25, val FP_INT: Int <> CONST = 4) extends EDDesign:
      val x         = SInt(FP_WIDTH) <> IN
      val y         = SInt(FP_WIDTH) <> OUT
      val leaf_inst = Leaf(W = FP_WIDTH, F = FP_WIDTH - FP_INT)
      leaf_inst.x <> x
      leaf_inst.y <> y

    class Top extends EDDesign:
      val x1     = SInt(25) <> IN
      val y1     = SInt(25) <> OUT
      val x2     = SInt(25) <> IN
      val y2     = SInt(25) <> OUT
      val inst_a = Mid(FP_WIDTH = 25, FP_INT = 4)
      val inst_b = Mid(FP_WIDTH = 25, FP_INT = 4)
      inst_a.x <> x1
      inst_a.y <> y1
      inst_b.x <> x2
      inst_b.y <> y2
    val id = (new Top).uniqueDesigns
    assertCodeString(
      id,
      """|class Leaf(
         |    val W: Int <> CONST = 8,
         |    val F: Int <> CONST = 4
         |) extends EDDesign:
         |  val x = SInt(W) <> IN
         |  val y = SInt(W) <> OUT
         |  process(all):
         |    y := x
         |end Leaf
         |
         |class Mid(
         |    val FP_WIDTH: Int <> CONST = 25,
         |    val FP_INT: Int <> CONST = 4
         |) extends EDDesign:
         |  val x = SInt(FP_WIDTH) <> IN
         |  val y = SInt(FP_WIDTH) <> OUT
         |  val leaf_inst = Leaf(
         |      W = FP_WIDTH,
         |      F = FP_WIDTH - FP_INT
         |  )
         |  leaf_inst.x <> x
         |  y <> leaf_inst.y
         |end Mid
         |
         |class Top extends EDDesign:
         |  val x1 = SInt(25) <> IN
         |  val y1 = SInt(25) <> OUT
         |  val x2 = SInt(25) <> IN
         |  val y2 = SInt(25) <> OUT
         |  val inst_a = Mid(
         |      FP_WIDTH = 25,
         |      FP_INT = 4
         |  )
         |  val inst_b = Mid(
         |      FP_WIDTH = 25,
         |      FP_INT = 4
         |  )
         |  inst_a.x <> x1
         |  y1 <> inst_a.y
         |  inst_b.x <> x2
         |  y2 <> inst_b.y
         |end Top
         |""".stripMargin
    )
  }
  test("Identical instances should share a single design") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class IDTop extends DFDesign:
      val x1  = SInt(16) <> IN
      val y1  = SInt(16) <> OUT
      val x2  = SInt(16) <> IN
      val y2  = SInt(16) <> OUT
      val id1 = new ID
      val id2 = new ID
      id1.x <> x1
      id1.y <> y1
      id2.x <> x2
      id2.y <> y2
    val id = (new IDTop).uniqueDesigns
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends DFDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val id1 = ID()
         |  val id2 = ID()
         |  id1.x <> x1
         |  y1 <> id1.y
         |  id2.x <> x2
         |  y2 <> id2.y
         |end IDTop
         |""".stripMargin
    )
  }
end UniqueDesignsSpec
