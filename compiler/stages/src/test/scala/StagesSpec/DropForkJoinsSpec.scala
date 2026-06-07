package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropForkJoins
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropForkJoinsSpec extends StageSpec:
  test("forkJoin (all) lowering under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class FJ extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        val fk = forkJoin:
          locally:
            a :== 1
          locally:
            b :== 1
    end FJ
    val fj = (new FJ).dropForkJoins
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val fk_start_0 = Bit <> VAR
         |  val fk_start_1 = Bit <> VAR
         |  val fk_done_0 = Bit <> VAR
         |  val fk_done_1 = Bit <> VAR
         |  process:
         |    fk_start_0 :== 1
         |    fk_start_1 :== 1
         |    waitUntil(fk_done_0 && fk_done_1)
         |    fk_start_0 :== 0
         |    fk_start_1 :== 0
         |  process:
         |    waitUntil(fk_start_0)
         |    locally:
         |      a :== 1
         |    fk_done_0 :== 1
         |    waitWhile(fk_start_0)
         |    fk_done_0 :== 0
         |  process:
         |    waitUntil(fk_start_1)
         |    locally:
         |      b :== 1
         |    fk_done_1 :== 1
         |    waitWhile(fk_start_1)
         |    fk_done_1 :== 0
         |end FJ
         |""".stripMargin
    )

  test("forkJoinAny lowering under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class FJ extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        val fk = forkJoinAny:
          locally:
            a :== 1
          locally:
            b :== 1
    end FJ
    val fj = (new FJ).dropForkJoins
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val fk_start_0 = Bit <> VAR
         |  val fk_start_1 = Bit <> VAR
         |  val fk_done_0 = Bit <> VAR
         |  val fk_done_1 = Bit <> VAR
         |  process:
         |    fk_start_0 :== 1
         |    fk_start_1 :== 1
         |    waitUntil(fk_done_0 || fk_done_1)
         |    fk_start_0 :== 0
         |    fk_start_1 :== 0
         |  process:
         |    waitUntil(fk_start_0)
         |    locally:
         |      a :== 1
         |    fk_done_0 :== 1
         |    waitWhile(fk_start_0)
         |    fk_done_0 :== 0
         |  process:
         |    waitUntil(fk_start_1)
         |    locally:
         |      b :== 1
         |    fk_done_1 :== 1
         |    waitWhile(fk_start_1)
         |    fk_done_1 :== 0
         |end FJ
         |""".stripMargin
    )

  test("forkJoinNone lowering under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class FJ extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        val fk = forkJoinNone:
          locally:
            a :== 1
          locally:
            b :== 1
    end FJ
    val fj = (new FJ).dropForkJoins
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val fk_start_0 = Bit <> VAR
         |  val fk_start_1 = Bit <> VAR
         |  val fk_done_0 = Bit <> VAR
         |  val fk_done_1 = Bit <> VAR
         |  process:
         |    fk_start_0 :== 1
         |    fk_start_1 :== 1
         |  process:
         |    waitUntil(fk_start_0)
         |    locally:
         |      a :== 1
         |    fk_done_0 :== 1
         |    waitWhile(fk_start_0)
         |    fk_done_0 :== 0
         |  process:
         |    waitUntil(fk_start_1)
         |    locally:
         |      b :== 1
         |    fk_done_1 :== 1
         |    waitWhile(fk_start_1)
         |    fk_done_1 :== 0
         |end FJ
         |""".stripMargin
    )

  test("nested fork lowering under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class FJ extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      val c = Bit <> OUT
      process:
        val fk = forkJoin:
          locally:
            a :== 1
          locally:
            val fkInner = forkJoin:
              locally:
                b :== 1
              locally:
                c :== 1
    end FJ
    val fj = (new FJ).dropForkJoins
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val c = Bit <> OUT
         |  val fkInner_start_0 = Bit <> VAR
         |  val fkInner_start_1 = Bit <> VAR
         |  val fkInner_done_0 = Bit <> VAR
         |  val fkInner_done_1 = Bit <> VAR
         |  val fk_start_0 = Bit <> VAR
         |  val fk_start_1 = Bit <> VAR
         |  val fk_done_0 = Bit <> VAR
         |  val fk_done_1 = Bit <> VAR
         |  process:
         |    fk_start_0 :== 1
         |    fk_start_1 :== 1
         |    waitUntil(fk_done_0 && fk_done_1)
         |    fk_start_0 :== 0
         |    fk_start_1 :== 0
         |  process:
         |    waitUntil(fk_start_0)
         |    locally:
         |      a :== 1
         |    fk_done_0 :== 1
         |    waitWhile(fk_start_0)
         |    fk_done_0 :== 0
         |  process:
         |    waitUntil(fk_start_1)
         |    locally:
         |      fkInner_start_0 :== 1
         |      fkInner_start_1 :== 1
         |      waitUntil(fkInner_done_0 && fkInner_done_1)
         |      fkInner_start_0 :== 0
         |      fkInner_start_1 :== 0
         |    fk_done_1 :== 1
         |    waitWhile(fk_start_1)
         |    fk_done_1 :== 0
         |  process:
         |    waitUntil(fkInner_start_0)
         |    locally:
         |      b :== 1
         |    fkInner_done_0 :== 1
         |    waitWhile(fkInner_start_0)
         |    fkInner_done_0 :== 0
         |  process:
         |    waitUntil(fkInner_start_1)
         |    locally:
         |      c :== 1
         |    fkInner_done_1 :== 1
         |    waitWhile(fkInner_start_1)
         |    fkInner_done_1 :== 0
         |end FJ
         |""".stripMargin
    )

  test("Verilog v2001 keeps forkJoin (all) native but lowers forkJoinAny"):
    given options.CompilerOptions.Backend = _.verilog.v2001
    class FJ extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        val fkAll = forkJoin:
          locally:
            a :== 1
          locally:
            b :== 1
        val fkAny = forkJoinAny:
          locally:
            a :== 0
          locally:
            b :== 0
    end FJ
    val fj = (new FJ).dropForkJoins
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val fkAny_start_0 = Bit <> VAR
         |  val fkAny_start_1 = Bit <> VAR
         |  val fkAny_done_0 = Bit <> VAR
         |  val fkAny_done_1 = Bit <> VAR
         |  process:
         |    val fkAll = forkJoin:
         |      locally:
         |        a :== 1
         |      locally:
         |        b :== 1
         |    fkAny_start_0 :== 1
         |    fkAny_start_1 :== 1
         |    waitUntil(fkAny_done_0 || fkAny_done_1)
         |    fkAny_start_0 :== 0
         |    fkAny_start_1 :== 0
         |  process:
         |    waitUntil(fkAny_start_0)
         |    locally:
         |      a :== 0
         |    fkAny_done_0 :== 1
         |    waitWhile(fkAny_start_0)
         |    fkAny_done_0 :== 0
         |  process:
         |    waitUntil(fkAny_start_1)
         |    locally:
         |      b :== 0
         |    fkAny_done_1 :== 1
         |    waitWhile(fkAny_start_1)
         |    fkAny_done_1 :== 0
         |end FJ
         |""".stripMargin
    )
end DropForkJoinsSpec
