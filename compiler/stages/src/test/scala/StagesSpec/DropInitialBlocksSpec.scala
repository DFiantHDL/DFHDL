package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropInitialBlocks
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropInitialBlocksSpec extends StageSpec(stageCreatesUnrefAnons = true):
  given options.CompilerOptions.Backend = _.vhdl

  test("a multi-statement group becomes a generated init function") {
    class Top extends EDDesign:
      val x   = SInt(16)     <> IN
      val y   = SInt(16)     <> OUT
      val vec = SInt(16) X 4 <> VAR
      initial:
        for (i <- 0 until 4)
          vec(i) := 0
      process(all):
        y :== x + vec(0)
    end Top
    val top = (new Top).dropInitialBlocks
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val vec = SInt(16) X 4 <> VAR init vec_init()
         |  def vec_init(): SInt[16] X 4 <> CONSTRET =
         |    val vec = SInt(16) X 4 <> VAR
         |    for (i <- 0 until 4)
         |      vec(i) := sd"16'0"
         |    end for
         |    vec
         |  end vec_init
         |
         |  process(all):
         |    y :== x + vec(0)
         |end Top
         |""".stripMargin
    )
  }

  test("a mixed block splits into a decl init and an init function") {
    class Top extends EDDesign:
      val x   = SInt(16)     <> IN
      val y   = SInt(16)     <> OUT
      val v   = SInt(16)     <> VAR
      val vec = SInt(16) X 4 <> VAR
      initial:
        v := 1
        for (i <- 0 until 4)
          vec(i) := 0
      process(all):
        y :== x + v + vec(0)
    end Top
    val top      = (new Top).dropInitialBlocks
    val expected =
      """|class Top extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init sd"16'1"
         |  val vec = SInt(16) X 4 <> VAR init vec_init()
         |  def vec_init(): SInt[16] X 4 <> CONSTRET =
         |    val vec = SInt(16) X 4 <> VAR
         |    for (i <- 0 until 4)
         |      vec(i) := sd"16'0"
         |    end for
         |    vec
         |  end vec_init
         |
         |  process(all):
         |    y :== x + v + vec(0)
         |end Top
         |""".stripMargin
    assertCodeString(top, expected)
    // fix-point: a second application leaves the transformed DB unchanged
    assertCodeString(top.dropInitialBlocks, expected)
  }

  test("RT without a reset converts to an init function with a captured constant") {
    class Top extends RTDesign:
      val EN: Boolean <> CONST = true
      val x                    = SInt(16)     <> IN
      val y                    = SInt(16)     <> OUT
      val vec                  = SInt(16) X 4 <> VAR
      initial:
        for (i <- 0 until 4)
          if (EN) vec(i) := 0
          else vec(i)    := 1
      y := x + vec(0)
    end Top
    val top = (new Top).dropInitialBlocks
    assertCodeString(
      top,
      """|class Top extends RTDesign:
         |  val EN: Boolean <> CONST = true
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val vec = SInt(16) X 4 <> VAR init vec_init()
         |  def vec_init(): SInt[16] X 4 <> CONSTRET =
         |    val vec = SInt(16) X 4 <> VAR
         |    for (i <- 0 until 4)
         |      if (EN) vec(i) := sd"16'0"
         |      else vec(i) := sd"16'1"
         |    end for
         |    vec
         |  end vec_init
         |
         |  y := x + vec(0)
         |end Top
         |""".stripMargin
    )
  }

  test("a self-reading single-declaration block converts to an init function") {
    class Top extends EDDesign:
      val y = SInt(16) <> OUT
      val v = SInt(16) <> VAR
      initial:
        v := 0
        v := v + 1
      process(all):
        y :== v
    end Top
    val top = (new Top).dropInitialBlocks
    // reads of `v` redirect to the function's local variable, preserving the in-block
    // sequential order (VHDL variable semantics)
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init v_init()
         |  def v_init(): SInt[16] <> CONSTRET =
         |    val v = SInt(16) <> VAR
         |    v := sd"16'0"
         |    v := v + sd"16'1"
         |    v
         |  end v_init
         |
         |  process(all):
         |    y :== v
         |end Top
         |""".stripMargin
    )
  }

  test("sim content becomes an endless-wait one-shot process") {
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
    val top = (new Top).dropInitialBlocks
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init sd"16'0"
         |  process:
         |    println(s"hello")
         |    wait
         |  process(all):
         |    y :== x + v
         |end Top
         |""".stripMargin
    )
  }

  test("a cross-reading multi-declaration block becomes an endless-wait process as a whole") {
    class Top extends EDDesign:
      val v = SInt(16) <> VAR
      val w = SInt(16) <> OUT
      initial:
        v := 0
        w := v + 1
    end Top
    val top = (new Top).dropInitialBlocks
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val v = SInt(16) <> VAR
         |  val w = SInt(16) <> OUT
         |  process:
         |    v := sd"16'0"
         |    w := v + sd"16'1"
         |    wait
         |end Top
         |""".stripMargin
    )
  }
end DropInitialBlocksSpec
