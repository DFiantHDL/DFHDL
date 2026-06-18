package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{dropForkJoinsED, dropForkJoinsRT}
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
    val fj = (new FJ).dropForkJoinsED
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
    val fj = (new FJ).dropForkJoinsED
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

  // join-any / join-none can dynamically spawn threads and are never lowered: a join-all in the
  // same design is lowered while the others are left as native fork blocks (rejected later by any
  // backend without native support).
  test("forkJoinAny / forkJoinNone are left untouched under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
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
        val fkNone = forkJoinNone:
          locally:
            a :== 1
          locally:
            b :== 0
    end FJ
    val fj = (new FJ).dropForkJoinsED
    assertCodeString(
      fj,
      """|class FJ extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val fkAll_start_0 = Bit <> VAR
         |  val fkAll_start_1 = Bit <> VAR
         |  val fkAll_done_0 = Bit <> VAR
         |  val fkAll_done_1 = Bit <> VAR
         |  process:
         |    fkAll_start_0 :== 1
         |    fkAll_start_1 :== 1
         |    waitUntil(fkAll_done_0 && fkAll_done_1)
         |    fkAll_start_0 :== 0
         |    fkAll_start_1 :== 0
         |    val fkAny = forkJoinAny:
         |      locally:
         |        a :== 0
         |      locally:
         |        b :== 0
         |    val fkNone = forkJoinNone:
         |      locally:
         |        a :== 1
         |      locally:
         |        b :== 0
         |  process:
         |    waitUntil(fkAll_start_0)
         |    locally:
         |      a :== 1
         |    fkAll_done_0 :== 1
         |    waitWhile(fkAll_start_0)
         |    fkAll_done_0 :== 0
         |  process:
         |    waitUntil(fkAll_start_1)
         |    locally:
         |      b :== 1
         |    fkAll_done_1 :== 1
         |    waitWhile(fkAll_start_1)
         |    fkAll_done_1 :== 0
         |end FJ
         |""".stripMargin
    )

  test("forkJoin (all) lowering under RT"):
    class FJ extends RTDesign:
      val a = Bit <> OUT.REG
      val b = Bit <> OUT.REG
      process:
        val fk = forkJoin:
          locally:
            a.din := 1
          locally:
            b.din := 1
    end FJ
    val fj = (new FJ).dropForkJoinsRT
    assertCodeString(
      fj,
      """|class FJ extends RTDesign:
         |  val a = Bit <> OUT.REG
         |  val b = Bit <> OUT.REG
         |  val fk_start_0 = Bit <> VAR.REG
         |  val fk_start_1 = Bit <> VAR.REG
         |  val fk_done_0 = Bit <> VAR.REG
         |  val fk_done_1 = Bit <> VAR.REG
         |  process:
         |    fk_start_0.din := 1
         |    fk_start_1.din := 1
         |    waitUntil(fk_done_0 && fk_done_1)
         |    fk_start_0.din := 0
         |    fk_start_1.din := 0
         |  process:
         |    waitUntil(fk_start_0)
         |    locally:
         |      a.din := 1
         |    fk_done_0.din := 1
         |    waitWhile(fk_start_0)
         |    fk_done_0.din := 0
         |  process:
         |    waitUntil(fk_start_1)
         |    locally:
         |      b.din := 1
         |    fk_done_1.din := 1
         |    waitWhile(fk_start_1)
         |    fk_done_1.din := 0
         |end FJ
         |""".stripMargin
    )

end DropForkJoinsSpec
