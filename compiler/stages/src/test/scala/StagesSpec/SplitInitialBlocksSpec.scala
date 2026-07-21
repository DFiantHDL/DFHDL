package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.splitInitialBlocks
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class SplitInitialBlocksSpec extends StageSpec(stageCreatesUnrefAnons = true):
  val timingAnnots =
    """|@timing.clock(rate = 50.MHz, edge = _.rising, portName = "clk", inclusionPolicy = _.asneeded, grpName = "default")
       |@timing.reset(mode = _.sync, active = _.high, portName = "rst", inclusionPolicy = _.asneeded)""".stripMargin

  test("per-variable split and init conversion under RT with reset") {
    class Top extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG
      val v = SInt(16) <> VAR
      initial:
        y.din := 0
        v     := 1
      y.din := x + v
    end Top
    val top = (new Top).splitInitialBlocks
    assertCodeString(
      top,
      s"""|$timingAnnots
          |class Top extends RTDesign:
          |  val x = SInt(16) <> IN
          |  val y = SInt(16) <> OUT.REG init sd"16'0"
          |  val v = SInt(16) <> VAR init sd"16'1"
          |  y.din := x + v
          |end Top
          |""".stripMargin
    )
  }

  test("non-convertible per-variable block stays initial") {
    class Top extends RTDesign:
      val x   = SInt(16)     <> IN
      val y   = SInt(16)     <> OUT.REG
      val vec = SInt(16) X 4 <> VAR
      initial:
        y.din := 0
        for (i <- 0 until 4)
          vec(i) := 0
      y.din := x + vec(0)
    end Top
    val top = (new Top).splitInitialBlocks
    assertCodeString(
      top,
      s"""|$timingAnnots
          |class Top extends RTDesign:
          |  val x = SInt(16) <> IN
          |  val y = SInt(16) <> OUT.REG init sd"16'0"
          |  val vec = SInt(16) X 4 <> VAR
          |  initial:
          |    for (i <- 0 until 4)
          |      vec(i) := sd"16'0"
          |    end for
          |  y.din := x + vec(0)
          |end Top
          |""".stripMargin
    )
  }

  test("untouched under RT without a reset") {
    class Top extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val v = SInt(16) <> VAR
      initial:
        v := 1
      y := x + v
    end Top
    val top = (new Top).splitInitialBlocks
    assertCodeString(
      top,
      """|class Top extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  initial:
         |    v := sd"16'1"
         |  y := x + v
         |end Top
         |""".stripMargin
    )
  }

  test("VHDL: assignments become inits and sim content stays initial") {
    given options.CompilerOptions.Backend = _.vhdl
    class Top extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val v = SInt(16) <> VAR
      initial:
        v := 0
        println("hello")
      process(all):
        y :== x + v
    end Top
    val top = (new Top).splitInitialBlocks
    // the residual simulation-only block is left initial for `DropInitialBlocks` to lower
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init sd"16'0"
         |  initial:
         |    println(s"hello")
         |  process(all):
         |    y :== x + v
         |end Top
         |""".stripMargin
    )
  }

  test("VHDL: a cross-reading initial block is left whole") {
    given options.CompilerOptions.Backend = _.vhdl
    class Top extends EDDesign:
      val v = SInt(16) <> VAR
      val w = SInt(16) <> OUT
      initial:
        v := 0
        w := v + 1
    end Top
    val top = (new Top).splitInitialBlocks
    // splitting would lose the intra-block order; `DropInitialBlocks` lowers it whole
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val v = SInt(16) <> VAR
         |  val w = SInt(16) <> OUT
         |  initial:
         |    v := sd"16'0"
         |    w := v + sd"16'1"
         |end Top
         |""".stripMargin
    )
  }

  test("conditional split keeps only each variable's chain") {
    class Top extends RTDesign:
      val EN: Boolean <> CONST = true
      val y                    = SInt(16) <> OUT.REG
      val v                    = SInt(16) <> VAR
      initial:
        if (EN)
          y.din := 0
          v     := 1
        else y.din := 2
      y.din := v
    end Top
    val top = (new Top).splitInitialBlocks
    assertCodeString(
      top,
      s"""|$timingAnnots
          |class Top extends RTDesign:
          |  val EN: Boolean <> CONST = true
          |  val y = SInt(16) <> OUT.REG
          |  val v = SInt(16) <> VAR
          |  initial:
          |    if (EN) y.din := sd"16'0"
          |    else y.din := sd"16'2"
          |  initial:
          |    if (EN) v := sd"16'1"
          |  y.din := v
          |end Top
          |""".stripMargin
    )
  }
end SplitInitialBlocksSpec
