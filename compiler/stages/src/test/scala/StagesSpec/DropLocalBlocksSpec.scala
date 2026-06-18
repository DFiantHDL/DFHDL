package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{dropLocalBlocksED, dropLocalBlocksRT}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropLocalBlocksSpec extends StageSpec:
  test("standalone local block flattening under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class LB extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        a :== 1
        locally:
          b :== 1
          a :== 0
    end LB
    val lb = (new LB).dropLocalBlocksED
    assertCodeString(
      lb,
      """|class LB extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  process:
         |    a :== 1
         |    b :== 1
         |    a :== 0
         |end LB
         |""".stripMargin
    )

  test("nested local blocks flattening under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class LB extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      val c = Bit <> OUT
      process:
        a :== 1
        locally:
          b :== 1
          locally:
            c :== 1
    end LB
    val lb = (new LB).dropLocalBlocksED
    assertCodeString(
      lb,
      """|class LB extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  val c = Bit <> OUT
         |  process:
         |    a :== 1
         |    b :== 1
         |    c :== 1
         |end LB
         |""".stripMargin
    )

  test("local block keeps under Verilog"):
    given options.CompilerOptions.Backend = _.verilog
    class LB extends EDDesign:
      val a = Bit <> OUT
      val b = Bit <> OUT
      process:
        a :== 1
        locally:
          b :== 1
          a :== 0
    end LB
    val lb = (new LB).dropLocalBlocksED
    assertCodeString(
      lb,
      """|class LB extends EDDesign:
         |  val a = Bit <> OUT
         |  val b = Bit <> OUT
         |  process:
         |    a :== 1
         |    locally:
         |      b :== 1
         |      a :== 0
         |end LB
         |""".stripMargin
    )

  test("standalone local block flattening under RT"):
    class LB extends RTDesign:
      val a = Bit <> OUT.REG
      val b = Bit <> OUT.REG
      process:
        a.din := 1
        locally:
          b.din := 1
          a.din := 0
    end LB
    val lb = (new LB).dropLocalBlocksRT
    assertCodeString(
      lb,
      """|class LB extends RTDesign:
         |  val a = Bit <> OUT.REG
         |  val b = Bit <> OUT.REG
         |  process:
         |    a.din := 1
         |    b.din := 1
         |    a.din := 0
         |end LB
         |""".stripMargin
    )

  test("nested local blocks flattening under RT"):
    class LB extends RTDesign:
      val a = Bit <> OUT.REG
      val b = Bit <> OUT.REG
      val c = Bit <> OUT.REG
      process:
        a.din := 1
        locally:
          b.din := 1
          locally:
            c.din := 1
    end LB
    val lb = (new LB).dropLocalBlocksRT
    assertCodeString(
      lb,
      """|class LB extends RTDesign:
         |  val a = Bit <> OUT.REG
         |  val b = Bit <> OUT.REG
         |  val c = Bit <> OUT.REG
         |  process:
         |    a.din := 1
         |    b.din := 1
         |    c.din := 1
         |end LB
         |""".stripMargin
    )
end DropLocalBlocksSpec
