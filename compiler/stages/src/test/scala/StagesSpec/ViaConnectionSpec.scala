package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.viaConnection
import dfhdl.hdl.DFDesign
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ViaConnectionSpec extends StageSpec:
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
         |  val id1 = new ID:
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2_x = SInt(16) <> VAR
         |  val id2_y = SInt(16) <> VAR
         |  val id2 = new ID:
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

    class IDTop extends DFDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val id = new IDExtra
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
         |end IDExtra
         |
         |class IDTop extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id_x = SInt(16) <> VAR
         |  val id_y = SInt(16) <> VAR
         |  val id = new IDExtra:
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
      val id1 = new ID:
        this.x <> id1_x
        this.y <> id1_y
      val id2 = new ID:
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
         |  val id1 = new ID:
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2 = new ID:
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
         |  val id1 = new ID:
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ internal
         |  val id2_y = SInt(16) <> VAR
         |  val id2 = new ID:
         |    this.y <>/*-->*/ id2_y
         |    this.x <>/*<--*/ internal
         |  id1_x <> x
         |  y <> id2_y
         |end IDTopVar
         |""".stripMargin
    )
  }
end ViaConnectionSpec
