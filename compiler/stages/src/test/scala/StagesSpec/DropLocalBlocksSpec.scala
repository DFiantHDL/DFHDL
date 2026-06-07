package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropLocalBlocks
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
    val lb = (new LB).dropLocalBlocks
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
    val lb = (new LB).dropLocalBlocks
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
    val lb = (new LB).dropLocalBlocks
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
end DropLocalBlocksSpec
