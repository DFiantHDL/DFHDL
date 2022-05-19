package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{sanityCheck, getCodeString}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintCodeStringSpec extends StageSpec:
  class ID extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y := x

  class IDGen[T <: DFType](dfType: T) extends DFDesign:
    val x = dfType <> IN
    val y = dfType <> OUT
    y := x

  class IDTop extends DFDesign:
    val x   = SInt(16) <> IN
    val y   = SInt(16) <> OUT
    val id1 = new ID
    val id2 = new ID
    id1.x <> x
    id1.y <> id2.x
    id2.y <> y

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

  test("Basic ID design") {
    val id = (new ID).getCodeString
    assertNoDiff(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Generic ID design") {
    val id = (new IDGen(Bits(8))).getCodeString
    assertNoDiff(
      id,
      """|class IDGen extends DFDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  y := x
         |end IDGen
         |""".stripMargin
    )
    val id2 = (new IDGen((Bits(4), SInt(4)))).getCodeString
    assertNoDiff(
      id2,
      """|class IDGen extends DFDesign:
         |  val x = (Bits(4), SInt(4)) <> IN
         |  val y = (Bits(4), SInt(4)) <> OUT
         |  y := x
         |end IDGen
         |""".stripMargin
    )
  }
  test("Basic ID design hierarchy") {
    val id = (new IDTop).getCodeString
    assertNoDiff(
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
         |  val id1 = new ID
         |  val id2 = new ID
         |  id1.x <> x
         |  id2.x <> id1.y
         |  y <> id2.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Via-connection ID design hierarchy") {
    val id = (new IDTopVia).getCodeString
    assertNoDiff(
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
  test("Design names affect named dataflow types") {
    object ID extends Opaque(Bit)
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = ID       <> VAR init 0.as(ID)
      y := x
    val id = (new ID).getCodeString
    assertNoDiff(
      id,
      """|class ID extends DFDesign:
         |  object ID_0 extends Opaque(Bit)
         |
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = ID_0 <> VAR init 0.as(ID_0)
         |  y := x
         |end ID
         |""".stripMargin
    )
  }

  test("Basic RTDesign") {
    class ID extends RTDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val flag = Bit      <> IN
      y := x.reg.reg(2) - x
    end ID
    val id = (new ID).getCodeString
    assertNoDiff(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val flag = Bit <> IN
         |  y := x.reg.reg(2) - x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic EDDesign") {
    class ID extends EDDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val flag = Bit      <> IN
      val v    = SInt(16) <> VAR
      process.forever {
        val z = SInt(8) <> VAR
        z := z + 1
        v :== 1
      }
      process(x, y, flag.rising) {
        val z = SInt(8) <> VAR
        z := 1
      }
      process(all) {
        val z = SInt(8) <> VAR
        object Bla:
          val z = Bits(8) <> VAR
          z := all(0)
        Bla.z
        z := 1
      }
    end ID
    val id = (new ID).getCodeString
    assertNoDiff(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val flag = Bit <> IN
         |  val v = SInt(16) <> VAR
         |  process.forever {
         |    val z = SInt(8) <> VAR
         |    z := z + sd"2'1"
         |    v :== sd"16'1"
         |  }
         |  process(x, y, flag.rising) {
         |    val z = SInt(8) <> VAR
         |    z := sd"8'1"
         |  }
         |  process(all) {
         |    val z_0 = SInt(8) <> VAR
         |    val z_1 = Bits(8) <> VAR
         |    z_1 := h"00"
         |    z_0 := sd"8'1"
         |  }
         |end ID
         |""".stripMargin
    )
  }
  test("Domains") {
    class IDWithDomains extends DFDesign:
      val y = SInt(16) <> OUT
      val fast = new RTDomain:
        val pr = SInt(16) <> REG
        val pw = SInt(16) <> WIRE
        pr.din := 1
      y := fast.pr
      val fastdf = new DFDomain:
        val p = SInt(16) <> VAR
        p := 1
      fast.pw := fastdf.p
    val id = (new IDWithDomains).getCodeString
    assertNoDiff(
      id,
      """|class IDWithDomains extends DFDesign:
         |  val y = SInt(16) <> OUT
         |  val fast = new RTDomain:
         |    val pr = SInt(16) <> REG
         |    val pw = SInt(16) <> WIRE
         |    pr.din := sd"16'1"
         |  y := fast.pr
         |  val fastdf = new DFDomain:
         |    val p = SInt(16) <> VAR
         |    p := sd"16'1"
         |  fast.pw := fastdf.p
         |end IDWithDomains
         |""".stripMargin
    )
  }
end PrintCodeStringSpec
