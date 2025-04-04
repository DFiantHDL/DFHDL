package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.viaConnection
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ViaConnectionSpec extends StageSpec(stageCreatesUnrefAnons = true):
  class ID extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y := x

  test("Basic ID design") {
    val id = (new ID).viaConnection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic ID design hierarchy") {
    class IDTop extends DFDesign:
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val id1 = new ID
      val id2 = new ID
      id1.x <> x
      id1.y <> id2.x
      id2.y <> y

    val id = (new IDTop).viaConnection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id1_x = SInt(16) <> VAR
         |  val id1_y = SInt(16) <> VAR
         |  val id1 = new ID():
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2_x = SInt(16) <> VAR
         |  val id2_y = SInt(16) <> VAR
         |  val id2 = new ID():
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ id2_y
         |  id1_x <> x
         |  id2_x <> id1_y
         |  y <> id2_y
         |end IDTop
         |""".stripMargin
    )
  }
  test("ID design hierarchy with unused output") {
    class IDExtra extends DFDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val y2 = SInt(16) <> OUT
      y2 := x
      y  := y2

    class IDTop extends DFDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val id = new IDExtra()
      id.x  <> x
      id.y  <> y
      id.y2 <> OPEN

    val id = (new IDTop).viaConnection
    assertCodeString(
      id,
      """|class IDExtra extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val y2 = SInt(16) <> OUT
         |  y2 := x
         |  y := y2
         |end IDExtra
         |
         |class IDTop extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id_x = SInt(16) <> VAR
         |  val id_y = SInt(16) <> VAR
         |  val id = new IDExtra():
         |    this.x <>/*<--*/ id_x
         |    this.y <>/*-->*/ id_y
         |    this.y2 <>/*-->*/ OPEN
         |  id_x <> x
         |  y <> id_y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Via-connection ID design hierarchy") {
    class IDTopVia extends DFDesign:
      self =>
      val x     = SInt(16) <> IN
      val y     = SInt(16) <> OUT
      val id1_x = SInt(16) <> VAR
      val id1_y = SInt(16) <> VAR
      val id2_x = SInt(16) <> VAR
      val id2_y = SInt(16) <> VAR
      val id1 = new ID():
        this.x <> id1_x
        this.y <> id1_y
      val id2 = new ID():
        this.x <> id2_x
        this.y <> id2_y
      x     <> id1_x
      id1_y <> id2_x
      y     <> id2_y
    end IDTopVia

    val id = (new IDTopVia).viaConnection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTopVia extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id1_x = SInt(16) <> VAR
         |  val id1_y = SInt(16) <> VAR
         |  val id2_x = SInt(16) <> VAR
         |  val id2_y = SInt(16) <> VAR
         |  val id1 = new ID():
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2 = new ID():
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ id2_y
         |  id1_x <> x
         |  id2_x <> id1_y
         |  y <> id2_y
         |end IDTopVia
         |""".stripMargin
    )
  }

  test("Var intermediate connection design hierarchy") {
    class IDTopVar extends DFDesign:
      val x        = SInt(16) <> IN
      val y        = SInt(16) <> OUT
      val internal = SInt(16) <> VAR
      val id1      = new ID
      val id2      = new ID
      id1.x <> x
      id1.y <> internal
      id2.x <> internal
      id2.y <> y

    val id = (new IDTopVar).viaConnection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTopVar extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val internal = SInt(16) <> VAR
         |  val id1_x = SInt(16) <> VAR
         |  val id1 = new ID():
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ internal
         |  val id2_y = SInt(16) <> VAR
         |  val id2 = new ID():
         |    this.y <>/*-->*/ id2_y
         |    this.x <>/*<--*/ internal
         |  id1_x <> x
         |  y <> id2_y
         |end IDTopVar
         |""".stripMargin
    )
  }

  test("Via connection with partial selection") {
    class ID extends DFDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      y := x

    class IDTopVar extends DFDesign:
      val x   = Bits(16) <> IN
      val y   = Bits(16) <> OUT
      val id1 = new ID
      val id2 = new ID
      id1.x <> x(7, 0)
      id2.x <> x(15, 8)
      id1.y <> y(7, 0)
      id2.y <> y(15, 8)

    val id = (new IDTopVar).viaConnection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  y := x
         |end ID
         |
         |class IDTopVar extends DFDesign:
         |  val x = Bits(16) <> IN
         |  val y = Bits(16) <> OUT
         |  val id1_x = Bits(8) <> VAR
         |  val id1_y = Bits(8) <> VAR
         |  val id1 = new ID():
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2_x = Bits(8) <> VAR
         |  val id2_y = Bits(8) <> VAR
         |  val id2 = new ID():
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ id2_y
         |  id1_x <> x(7, 0)
         |  id2_x <> x(15, 8)
         |  y(7, 0) <> id1_y
         |  y(15, 8) <> id2_y
         |end IDTopVar
         |""".stripMargin
    )
  }
  test("Via connection with partial selection 2") {
    class ID extends EDDesign:
      val x = Bits(3) <> IN
      val y = Bits(3) <> OUT
      y <> x

    class IDTop extends EDDesign:
      val x = Bits(3)     <> IN
      val y = Bits(3)     <> OUT
      val v = Bits(3) X 2 <> VAR
      // TODO: need to fix this edge-case that yields an error
//      val c   = Bits(3) X 1 <> VAR init all(all(0))
      val id1 = new ID
      val id2 = new ID
      id1.y <> v(0)
      id2.y <> v(1)
      id1.x <> x // c(0)
      id2.x <> x
      process(all):
        y := v(0) | v(1)
    end IDTop

    val id = (new IDTop).viaConnection
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = Bits(3) <> IN
         |  val y = Bits(3) <> OUT
         |  y <> x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = Bits(3) <> IN
         |  val y = Bits(3) <> OUT
         |  val v = Bits(3) X 2 <> VAR
         |  val id1_x = Bits(3) <> VAR
         |  val id1 = new ID():
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ v(0)
         |  val id2_x = Bits(3) <> VAR
         |  val id2 = new ID():
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ v(1)
         |  id1_x <> x
         |  id2_x <> x
         |  process(all):
         |    y := v(0) | v(1)
         |end IDTop
         |""".stripMargin
    )
  }

  test("Hierarchical design with parameters") {
    class ID(
        val width: Int <> CONST,
        val length: Int <> CONST
    ) extends EDDesign:
      val x = Bits(width) X length <> IN
      val y = Bits(width) X length <> OUT
      val v = Bits(width) X length <> VAR
      v <> x
      y <> v
    class IDTop(
        val widthTop: Int <> CONST  = 8,
        val lengthTop: Int <> CONST = 10
    ) extends EDDesign:
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

    val id = (new IDTop).viaConnection
    assertCodeString(
      id,
      """|class ID(
         |    val width: Int <> CONST,
         |    val length: Int <> CONST
         |) extends EDDesign:
         |  val x = Bits(width) X length <> IN
         |  val y = Bits(width) X length <> OUT
         |  val v = Bits(width) X length <> VAR
         |  v <> x
         |  y <> v
         |end ID
         |
         |class IDTop(
         |    val widthTop: Int <> CONST = 8,
         |    val lengthTop: Int <> CONST = 10
         |) extends EDDesign:
         |  val x1 = Bits(widthTop) X lengthTop <> IN
         |  val y1 = Bits(widthTop) X lengthTop <> OUT
         |  val id1_x = Bits(widthTop) X lengthTop <> VAR
         |  val id1_y = Bits(widthTop) X lengthTop <> VAR
         |  val id1 = new ID(
         |      width = widthTop,
         |      length = lengthTop
         |  ):
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  id1_x <> x1
         |  y1 <> id1_y
         |  val x2 = Bits(widthTop) X (lengthTop + 1) <> IN
         |  val y2 = Bits(widthTop) X (lengthTop + 1) <> OUT
         |  val id2_x = Bits(widthTop) X (lengthTop + 1) <> VAR
         |  val id2_y = Bits(widthTop) X (lengthTop + 1) <> VAR
         |  val id2 = new ID(
         |      width = widthTop,
         |      length = lengthTop + 1
         |  ):
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ id2_y
         |  id2_x <> x2
         |  y2 <> id2_y
         |  val x3 = Bits(widthTop) X 7 <> IN
         |  val y3 = Bits(widthTop) X 7 <> OUT
         |  val id3_x = Bits(widthTop) X 7 <> VAR
         |  val id3_y = Bits(widthTop) X 7 <> VAR
         |  val id3 = new ID(
         |      width = widthTop,
         |      length = 7
         |  ):
         |    this.x <>/*<--*/ id3_x
         |    this.y <>/*-->*/ id3_y
         |  id3_x <> x3
         |  y3 <> id3_y
         |end IDTop""".stripMargin
    )
  }
end ViaConnectionSpec
