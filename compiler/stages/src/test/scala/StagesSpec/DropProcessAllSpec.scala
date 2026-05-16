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
end DropProcessAllSpec
