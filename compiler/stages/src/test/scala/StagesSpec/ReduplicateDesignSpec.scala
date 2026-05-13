package StagesSpec

import dfhdl.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.stages.{ReduplicateDesign, StageRunner, HasDB, db}
import dfhdl.options.CompilerOptions
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// Dummy concrete `ReduplicateDesign` used by this spec only: any DFDesignInst
// whose member name starts with `dup` matches.
case object ReduplicateNamedDup extends ReduplicateDesign:
  def criteria(inst: DFDesignInst)(using MemberGetSet, CompilerOptions): Boolean =
    inst.getName.startsWith("dup")

extension [T: HasDB](t: T)
  def reduplicateNamedDup(using CompilerOptions): DB =
    StageRunner.run(ReduplicateNamedDup)(t.db)

class ReduplicateDesignSpec extends StageSpec:
  test("K == N == 3 — first matching keeps the original (renamed), two clones produced") {
    class Inner extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := x

    class Top extends DFDesign:
      val in_a  = UInt(8) <> IN
      val out_a = UInt(8) <> OUT
      val in_b  = UInt(8) <> IN
      val out_b = UInt(8) <> OUT
      val in_c  = UInt(8) <> IN
      val out_c = UInt(8) <> OUT
      val dup_a = Inner()
      val dup_b = Inner()
      val dup_c = Inner()
      dup_a.x <> in_a
      dup_a.y <> out_a
      dup_b.x <> in_b
      dup_b.y <> out_b
      dup_c.x <> in_c
      dup_c.y <> out_c

    val id = (new Top).reduplicateNamedDup
    assertCodeString(
      id,
      """|class Inner_dup_a extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner_dup_a
         |
         |class Inner_dup_b extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner_dup_b
         |
         |class Inner_dup_c extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner_dup_c
         |
         |class Top extends DFDesign:
         |  val in_a = UInt(8) <> IN
         |  val out_a = UInt(8) <> OUT
         |  val in_b = UInt(8) <> IN
         |  val out_b = UInt(8) <> OUT
         |  val in_c = UInt(8) <> IN
         |  val out_c = UInt(8) <> OUT
         |  val dup_a = Inner_dup_a()
         |  val dup_b = Inner_dup_b()
         |  val dup_c = Inner_dup_c()
         |  dup_a.x <> in_a
         |  out_a <> dup_a.y
         |  dup_b.x <> in_b
         |  out_b <> dup_b.y
         |  dup_c.x <> in_c
         |  out_c <> dup_c.y
         |end Top
         |""".stripMargin
    )
  }

  test("K < N — only the matching inst gets a clone; non-matching share the original") {
    class Inner extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := x

    class Top extends DFDesign:
      val in_a  = UInt(8) <> IN
      val out_a = UInt(8) <> OUT
      val in_b  = UInt(8) <> IN
      val out_b = UInt(8) <> OUT
      val in_c  = UInt(8) <> IN
      val out_c = UInt(8) <> OUT
      val a     = Inner()
      val dup_b = Inner()
      val c     = Inner()
      a.x <> in_a
      a.y <> out_a
      dup_b.x <> in_b
      dup_b.y <> out_b
      c.x <> in_c
      c.y <> out_c

    val id = (new Top).reduplicateNamedDup
    assertCodeString(
      id,
      """|class Inner extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner
         |
         |class Inner_dup_b extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner_dup_b
         |
         |class Top extends DFDesign:
         |  val in_a = UInt(8) <> IN
         |  val out_a = UInt(8) <> OUT
         |  val in_b = UInt(8) <> IN
         |  val out_b = UInt(8) <> OUT
         |  val in_c = UInt(8) <> IN
         |  val out_c = UInt(8) <> OUT
         |  val a = Inner()
         |  val dup_b = Inner_dup_b()
         |  val c = Inner()
         |  a.x <> in_a
         |  out_a <> a.y
         |  dup_b.x <> in_b
         |  out_b <> dup_b.y
         |  c.x <> in_c
         |  out_c <> c.y
         |end Top
         |""".stripMargin
    )
  }

  test("K == N == 1 — single matching inst is left untouched (no rename, no clone)") {
    class Inner extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := x

    class Top extends DFDesign:
      val in_a  = UInt(8) <> IN
      val out_a = UInt(8) <> OUT
      val dup_a = Inner()
      dup_a.x <> in_a
      dup_a.y <> out_a

    val id = (new Top).reduplicateNamedDup
    assertCodeString(
      id,
      """|class Inner extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Inner
         |
         |class Top extends DFDesign:
         |  val in_a = UInt(8) <> IN
         |  val out_a = UInt(8) <> OUT
         |  val dup_a = Inner()
         |  dup_a.x <> in_a
         |  out_a <> dup_a.y
         |end Top
         |""".stripMargin
    )
  }

  test("Nested sub-tree clone — descendants are cloned per top-level clone") {
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

    val id = (new Top).reduplicateNamedDup
    // K==N==2 at the top level. The first matching inst (dup_a) keeps the
    // original Inner design block but it gets renamed to Inner_dup_a; dup_b
    // gets a fresh Inner_dup_b clone. Both top-level designs contain their
    // own nested `Leaf` (the clone produces a fresh nested Leaf for ref
    // uniqueness — `UniqueDesigns` re-runs downstream to suffix-uniquify
    // the two `Leaf` names).
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
         |class Leaf extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x
         |end Leaf
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
end ReduplicateDesignSpec
