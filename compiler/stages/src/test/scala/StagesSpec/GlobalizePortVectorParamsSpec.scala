package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.globalizePortVectorParams
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class GlobalizePortVectorParams extends StageSpec(stageCreatesUnrefAnons = true):
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
    val top = (new Foo).globalizePortVectorParams
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
  test("Various vector params are globalized, only ports are affected"):
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
    val top = (new Foo).globalizePortVectorParams
    assertCodeString(
      top,
      """|val Foo_length: Int <> CONST = 10
         |val Foo_width: Int <> CONST = 8
         |class Foo extends RTDesign:
         |  val x1 = Bits(Foo_width) X Foo_length <> IN
         |  val y1 = Bits(Foo_width) X Foo_length <> OUT
         |  val v1 = Bits(Foo_width) X Foo_length <> VAR
         |  v1 <> x1
         |  y1 <> v1
         |  val x2 = UInt(Foo_width) X (Foo_length + 1) <> IN
         |  val y2 = UInt(Foo_width) X (Foo_length + 1) <> OUT
         |  val v2 = UInt(Foo_width) X (Foo_length + 1) <> VAR
         |  v2 <> x2
         |  y2 <> v2
         |  val x3 = SInt(Foo_width) X 7 <> IN
         |  val y3 = SInt(Foo_width) X 7 <> OUT
         |  val v3 = SInt(Foo_width) X 7 <> VAR
         |  v3 <> x3
         |  y3 <> v3
         |  val x4 = Bits(Foo_width) X 7 X Foo_length <> IN
         |  val y4 = Bits(Foo_width) X 7 X Foo_length <> OUT
         |  val v4 = Bits(Foo_width) X 7 X Foo_length <> VAR
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
    val top = (new Foo).globalizePortVectorParams
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
  // TODO: needs to be fixed
  // test("Hierarchy parameter globalization"):
  //   class ID(
  //       val width: Int <> CONST,
  //       val length: Int <> CONST
  //   ) extends RTDesign:
  //     val x = Bits(width) X length <> IN
  //     val y = Bits(width) X length <> OUT
  //     val v = Bits(width) X length <> VAR
  //     v <> x
  //     y <> v
  //   class IDTop(
  //       val width: Int <> CONST  = 8,
  //       val length: Int <> CONST = 10
  //   ) extends RTDesign:
  //     val x1  = Bits(width) X length <> IN
  //     val y1  = Bits(width) X length <> OUT
  //     val id1 = ID(width, length)
  //     id1.x <> x1
  //     y1    <> id1.y
  //     val x2  = Bits(width) X length <> IN
  //     val y2  = Bits(width) X length <> OUT
  //     val id2 = ID(width, length)
  //     id2.x <> x2
  //     y2    <> id2.y
  //   end IDTop
  //   val top = (new IDTop()).globalizePortVectorParams
  //   assertCodeString(
  //     top,
  //     """|""".stripMargin
  //   )
end GlobalizePortVectorParams