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
  test("Hierarchy parameter globalization"):
    class ID(
        val width: Int <> CONST,
        val length: Int <> CONST
    ) extends RTDesign:
      val x = Bits(width) X length <> IN
      val y = Bits(width) X length <> OUT
      val v = Bits(width) X length <> VAR
      v <> x
      y <> v
    class IDTop(
        val widthTop: Int <> CONST  = 8,
        val lengthTop: Int <> CONST = 10
    ) extends RTDesign:
      val x1  = Bits(widthTop) X lengthTop <> IN
      val y1  = Bits(widthTop) X lengthTop <> OUT
      val id1 = ID(widthTop, lengthTop)
      id1.x <> x1
      y1    <> id1.y
      val x2  = Bits(widthTop) X (lengthTop + 1) <> IN
      val y2  = Bits(widthTop) X (lengthTop + 1) <> OUT
      val id2 = ID(widthTop, lengthTop + 1)
      id2.x <> x2
      y2    <> id2.y
      val x3  = Bits(widthTop) X 7 <> IN
      val y3  = Bits(widthTop) X 7 <> OUT
      val id3 = ID(widthTop, 7)
      id3.x <> x3
      y3    <> id3.y
    end IDTop
    val top = (new IDTop()).globalizePortVectorParams
    assertCodeString(
      top,
      """|val IDTop_lengthTop: Int <> CONST = 10
         |val IDTop_widthTop: Int <> CONST = 8
         |val IDTop_id1_length: Int <> CONST = IDTop_lengthTop
         |val IDTop_id1_width: Int <> CONST = IDTop_widthTop
         |val IDTop_id2_length: Int <> CONST = IDTop_lengthTop + 1
         |val IDTop_id2_width: Int <> CONST = IDTop_widthTop
         |val IDTop_id3_length: Int <> CONST = 7
         |val IDTop_id3_width: Int <> CONST = IDTop_widthTop
         |class ID_IDTop_id1 extends RTDesign:
         |  val x = Bits(IDTop_id1_width) X IDTop_id1_length <> IN
         |  val y = Bits(IDTop_id1_width) X IDTop_id1_length <> OUT
         |  val v = Bits(IDTop_id1_width) X IDTop_id1_length <> VAR
         |  v <> x
         |  y <> v
         |end ID_IDTop_id1
         |
         |class ID_IDTop_id2 extends RTDesign:
         |  val x = Bits(IDTop_id2_width) X IDTop_id2_length <> IN
         |  val y = Bits(IDTop_id2_width) X IDTop_id2_length <> OUT
         |  val v = Bits(IDTop_id2_width) X IDTop_id2_length <> VAR
         |  v <> x
         |  y <> v
         |end ID_IDTop_id2
         |
         |class ID_IDTop_id3 extends RTDesign:
         |  val x = Bits(IDTop_id3_width) X IDTop_id3_length <> IN
         |  val y = Bits(IDTop_id3_width) X IDTop_id3_length <> OUT
         |  val v = Bits(IDTop_id3_width) X IDTop_id3_length <> VAR
         |  v <> x
         |  y <> v
         |end ID_IDTop_id3
         |
         |class IDTop extends RTDesign:
         |  val x1 = Bits(IDTop_id1_width) X IDTop_id1_length <> IN
         |  val y1 = Bits(IDTop_id1_width) X IDTop_id1_length <> OUT
         |  val id1 = ID_IDTop_id1()
         |  id1.x <> x1
         |  y1 <> id1.y
         |  val x2 = Bits(IDTop_id2_width) X IDTop_id2_length <> IN
         |  val y2 = Bits(IDTop_id2_width) X IDTop_id2_length <> OUT
         |  val id2 = ID_IDTop_id2()
         |  id2.x <> x2
         |  y2 <> id2.y
         |  val x3 = Bits(IDTop_id3_width) X IDTop_id3_length <> IN
         |  val y3 = Bits(IDTop_id3_width) X IDTop_id3_length <> OUT
         |  val id3 = ID_IDTop_id3()
         |  id3.x <> x3
         |  y3 <> id3.y
         |end IDTop""".stripMargin
    )

  test("Hierarchy parameter globalization with the same parameter names"):
    class ID(
        val width: Int <> CONST,
        val length: Int <> CONST
    ) extends RTDesign:
      val x = Bits(width) X length <> IN
      val y = Bits(width) X length <> OUT
      val v = Bits(width) X length <> VAR
      v <> x
      y <> v
    class IDTop(
        val width: Int <> CONST  = 8,
        val length: Int <> CONST = 10
    ) extends RTDesign:
      val x1  = Bits(width) X length <> IN
      val y1  = Bits(width) X length <> OUT
      val id1 = ID(width, length)
      id1.x <> x1
      y1    <> id1.y
      val x2  = Bits(width) X (length + 1) <> IN
      val y2  = Bits(width) X (length + 1) <> OUT
      val id2 = ID(width, length + 1)
      id2.x <> x2
      y2    <> id2.y
    end IDTop
    val top = (new IDTop()).globalizePortVectorParams
    assertCodeString(
      top,
      """|val IDTop_length: Int <> CONST = 10
         |val IDTop_width: Int <> CONST = 8
         |val IDTop_id1_length: Int <> CONST = IDTop_length
         |val IDTop_id1_width: Int <> CONST = IDTop_width
         |val IDTop_id2_length: Int <> CONST = IDTop_length + 1
         |val IDTop_id2_width: Int <> CONST = IDTop_width
         |class ID_IDTop_id1 extends RTDesign:
         |  val x = Bits(IDTop_id1_width) X IDTop_id1_length <> IN
         |  val y = Bits(IDTop_id1_width) X IDTop_id1_length <> OUT
         |  val v = Bits(IDTop_id1_width) X IDTop_id1_length <> VAR
         |  v <> x
         |  y <> v
         |end ID_IDTop_id1
         |
         |class ID_IDTop_id2 extends RTDesign:
         |  val x = Bits(IDTop_id2_width) X IDTop_id2_length <> IN
         |  val y = Bits(IDTop_id2_width) X IDTop_id2_length <> OUT
         |  val v = Bits(IDTop_id2_width) X IDTop_id2_length <> VAR
         |  v <> x
         |  y <> v
         |end ID_IDTop_id2
         |
         |class IDTop extends RTDesign:
         |  val x1 = Bits(IDTop_id1_width) X IDTop_id1_length <> IN
         |  val y1 = Bits(IDTop_id1_width) X IDTop_id1_length <> OUT
         |  val id1 = ID_IDTop_id1()
         |  id1.x <> x1
         |  y1 <> id1.y
         |  val x2 = Bits(IDTop_id2_width) X IDTop_id2_length <> IN
         |  val y2 = Bits(IDTop_id2_width) X IDTop_id2_length <> OUT
         |  val id2 = ID_IDTop_id2()
         |  id2.x <> x2
         |  y2 <> id2.y
         |end IDTop""".stripMargin
    )
end GlobalizePortVectorParams
