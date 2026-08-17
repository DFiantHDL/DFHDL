package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropProcessAll
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropProcessAllSpec extends StageSpec:
  given options.CompilerOptions.Backend = _.vhdl.v93
  test("Basic process"):
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process(all):
        y := x
    end ID
    val id = (new ID).dropProcessAll
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  process(x):
         |    y := x
         |end ID
         |""".stripMargin
    )
  test("Conditional blocks in process"):
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process(all):
        if (x == 0)
          y := 0
        else
          y := 1
    end ID
    val id = (new ID).dropProcessAll
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  process(x):
         |    if (x == sd"16'0") y := sd"16'0"
         |    else y := sd"16'1"
         |end ID
         |""".stripMargin
    )
  test("Conditional blocks in process + local variable"):
    class ID extends EDDesign:
      val x  = SInt(16) <> IN
      val x2 = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      process(all):
        val v = SInt(16) <> VAR
        v := 1
        if (x == 0)
          y := x2
        else
          y := v
    end ID
    val id = (new ID).dropProcessAll
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val x2 = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  process(x2, x):
         |    val v = SInt(16) <> VAR
         |    v := sd"16'1"
         |    if (x == sd"16'0") y := x2
         |    else y := v
         |end ID
         |""".stripMargin
    )
  test("Hierarchical dependency"):
    class ID extends EDDesign:
      val iBits = Bits(8) <> IN
      val oBits = Bits(8) <> OUT
      oBits <> iBits
    end ID

    class Foo extends EDDesign:
      val iBits    = Bits(8) <> IN
      val oBits    = Bits(8) <> OUT
      val dir      = Bit     <> IN
      val rshifter = ID()
      rshifter.iBits <> iBits
      process(all):
        if (dir) oBits := rshifter.oBits
        else oBits     := rshifter.oBits
    end Foo
    val top = (new Foo).dropProcessAll
    assertCodeString(
      top,
      """|class ID extends EDDesign:
         |  val iBits = Bits(8) <> IN
         |  val oBits = Bits(8) <> OUT
         |  oBits <> iBits
         |end ID
         |
         |class Foo extends EDDesign:
         |  val iBits = Bits(8) <> IN
         |  val oBits = Bits(8) <> OUT
         |  val dir = Bit <> IN
         |  val rshifter = ID()
         |  rshifter.iBits <> iBits
         |  process(rshifter.oBits, dir):
         |    if (dir) oBits := rshifter.oBits
         |    else oBits := rshifter.oBits
         |end Foo""".stripMargin
    )
  // a loop body's statements are the process's statements, so what they read must reach the
  // sensitivity list — as must the range that decides how often the loop runs
  test("Reads inside a loop"):
    class Top extends EDDesign:
      val x = Bits(8)     <> IN
      val v = Bits(8) X 4 <> VAR
      val y = Bits(8)     <> OUT
      process(all):
        for (i <- 0 until 4) v(i) := x
        y                         := v(0)
    end Top
    val top = (new Top).dropProcessAll
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val v = Bits(8) X 4 <> VAR
         |  process(x, v):
         |    for (i <- 0 until 4)
         |      v(i) := x
         |    end for
         |    y := v(0)
         |end Top
         |""".stripMargin
    )
  // A constant-index cell selection is exactly what the process is sensitive to, and unlike the
  // array it selects from it can be named in a Verilog event control.
  test("Constant-indexed array item under verilog.v95"):
    given options.CompilerOptions.Backend = _.verilog.v95
    class Top extends EDDesign:
      val x = Bits(8)     <> IN
      val v = Bits(8) X 4 <> VAR
      val y = Bits(8)     <> OUT
      process(all):
        v(0) := x
        y    := v(1)
    end Top
    val top = (new Top).dropProcessAll
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val v = Bits(8) X 4 <> VAR
         |  process(x, v(1)):
         |    v(0) := x
         |    y := v(1)
         |end Top
         |""".stripMargin
    )

  // a NON-constant index cannot name the cell that is read, so the whole array is the item — and
  // a Verilog event control cannot name an array (v95 has no `@*` either), so it is listed cell
  // by cell. VHDL names the array signal itself, which the loop test above pins.
  test("Dynamically indexed array item under verilog.v95"):
    given options.CompilerOptions.Backend = _.verilog.v95
    class Top extends EDDesign:
      val x   = Bits(8)     <> IN
      val idx = UInt(2)     <> IN
      val v   = Bits(8) X 4 <> VAR
      val y   = Bits(8)     <> OUT
      process(all):
        v(0) := x
        y    := v(idx)
    end Top
    val top = (new Top).dropProcessAll
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val idx = UInt(2) <> IN
         |  val y = Bits(8) <> OUT
         |  val v = Bits(8) X 4 <> VAR
         |  process(x, idx, v(0), v(1), v(2), v(3)):
         |    v(0) := x
         |    y := v(idx.toInt)
         |end Top
         |""".stripMargin
    )
end DropProcessAllSpec
