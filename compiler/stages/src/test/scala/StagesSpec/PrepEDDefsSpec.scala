package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.prepEDDefs
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrepEDDefsSpec extends StageSpec:
  given options.CompilerOptions.Backend = _.verilog.sv2009
  test("named ED function calls become variables") {
    class Top extends EDDesign:
      val a                                         = UInt(8) <> IN
      val y                                         = UInt(8) <> OUT
      val z                                         = UInt(8) <> OUT
      def add1(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + 1
      val x                                         = add1(a)
      y <> x
      process(all):
        val w = add1(a)
        z := w + w
    end Top
    val result = (new Top).prepEDDefs
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  def add1(l: UInt[8] <> VAL): UInt[8] <> EDRET =
         |    l + d"8'1"
         |  end add1
         |
         |  val a = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val z = UInt(8) <> OUT
         |  val x = UInt(8) <> VAR
         |  x <> add1(a)
         |  y <> x
         |  process(all):
         |    val w = UInt(8) <> VAR
         |    w := add1(a)
         |    z := w + w
         |end Top
         |""".stripMargin
    )
  }
end PrepEDDefsSpec
