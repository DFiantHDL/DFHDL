package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.inlinePortVectorParams
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class InlinePortVectorParamsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  given options.CompilerOptions.Backend = backends.vhdl.v93
  test("Various vector params are kept"):
    val width: Int <> CONST  = 8
    val length: Int <> CONST = 10
    class Foo extends RTDesign:
      val x1 = Bits(width) X length <> IN
      val y1 = Bits(width) X length <> OUT
      y1 <> x1
      val x2 = UInt(width) X (length + 1) <> IN
      val y2 = UInt(width) X (length + 1) <> OUT
      y2 <> x2
      val x3 = SInt(width) X 7 <> IN
      val y3 = SInt(width) X 7 <> OUT
      y3 <> x3
      val x4 = Bits(width) X 7 X length <> IN
      val y4 = Bits(width) X 7 X length <> OUT
      y4 <> x4
    val top = (new Foo).inlinePortVectorParams
    assertCodeString(
      top,
      """|val width: Int <> CONST = 8
         |val length: Int <> CONST = 10
         |class Foo extends RTDesign:
         |  val x1 = Bits(width) X length <> IN
         |  val y1 = Bits(width) X length <> OUT
         |  y1 <> x1
         |  val x2 = UInt(width) X (length + 1) <> IN
         |  val y2 = UInt(width) X (length + 1) <> OUT
         |  y2 <> x2
         |  val x3 = SInt(width) X 7 <> IN
         |  val y3 = SInt(width) X 7 <> OUT
         |  y3 <> x3
         |  val x4 = Bits(width) X 7 X length <> IN
         |  val y4 = Bits(width) X 7 X length <> OUT
         |  y4 <> x4
         |end Foo
         |""".stripMargin
    )
  test("Various vector params are inlined, only ports are affected"):
    class Foo(
        val width: Int <> CONST  = 8,
        val length: Int <> CONST = 10
    ) extends RTDesign:
      val x1 = Bits(width) X length <> IN
      val y1 = Bits(width) X length <> OUT
      val v1 = Bits(width) X length <> VAR
      v1 <> x1
      y1 <> v1
      val x2 = UInt(width) X (length + 1) <> IN
      val y2 = UInt(width) X (length + 1) <> OUT
      val v2 = UInt(width) X (length + 1) <> VAR
      v2 <> x2
      y2 <> v2
      val x3 = SInt(width) X 7 <> IN
      val y3 = SInt(width) X 7 <> OUT
      val v3 = SInt(width) X 7 <> VAR
      v3 <> x3
      y3 <> v3
      val x4 = Bits(width) X 7 X length <> IN
      val y4 = Bits(width) X 7 X length <> OUT
      val v4 = Bits(width) X 7 X length <> VAR
      v4 <> x4
      y4 <> v4
    end Foo
    val top = (new Foo).inlinePortVectorParams
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val length: Int <> CONST = 10
         |) extends RTDesign:
         |  val x1 = Bits(8) X 10 <> IN
         |  val y1 = Bits(8) X 10 <> OUT
         |  val v1 = Bits(width) X length <> VAR
         |  v1 <> x1
         |  y1 <> v1
         |  val x2 = UInt(8) X 11 <> IN
         |  val y2 = UInt(8) X 11 <> OUT
         |  val v2 = UInt(width) X (length + 1) <> VAR
         |  v2 <> x2
         |  y2 <> v2
         |  val x3 = SInt(8) X 7 <> IN
         |  val y3 = SInt(8) X 7 <> OUT
         |  val v3 = SInt(width) X 7 <> VAR
         |  v3 <> x3
         |  y3 <> v3
         |  val x4 = Bits(8) X 7 X 10 <> IN
         |  val y4 = Bits(8) X 7 X 10 <> OUT
         |  val v4 = Bits(width) X 7 X length <> VAR
         |  v4 <> x4
         |  y4 <> v4
         |end Foo
         |""".stripMargin
    )
  test("Various vector params are kept under vhdl.v2008"):
    given options.CompilerOptions.Backend = backends.vhdl.v2008
    class Foo(
        val width: Int <> CONST  = 8,
        val length: Int <> CONST = 10
    ) extends RTDesign:
      val x1 = Bits(width) X length <> IN
      val y1 = Bits(width) X length <> OUT
      y1 <> x1
      val x2 = UInt(width) X (length + 1) <> IN
      val y2 = UInt(width) X (length + 1) <> OUT
      y2 <> x2
      val x3 = SInt(width) X 7 <> IN
      val y3 = SInt(width) X 7 <> OUT
      y3 <> x3
      val x4 = Bits(width) X 7 X length <> IN
      val y4 = Bits(width) X 7 X length <> OUT
      y4 <> x4
    end Foo
    val top = (new Foo).inlinePortVectorParams
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val length: Int <> CONST = 10
         |) extends RTDesign:
         |  val x1 = Bits(width) X length <> IN
         |  val y1 = Bits(width) X length <> OUT
         |  y1 <> x1
         |  val x2 = UInt(width) X (length + 1) <> IN
         |  val y2 = UInt(width) X (length + 1) <> OUT
         |  y2 <> x2
         |  val x3 = SInt(width) X 7 <> IN
         |  val y3 = SInt(width) X 7 <> OUT
         |  y3 <> x3
         |  val x4 = Bits(width) X 7 X length <> IN
         |  val y4 = Bits(width) X 7 X length <> OUT
         |  y4 <> x4
         |end Foo
         |""".stripMargin
    )
end InlinePortVectorParamsSpec
