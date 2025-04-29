package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{getCodeString, sanityCheck}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintCodeStringSpec extends StageSpec:
  class ID(arg: Bit <> CONST = 1) extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y := x
  object ID:
    def apply()(using DFC): ID =
      new ID(0)
  class IDGen[T <: DFType](dfType: T) extends DFDesign:
    val x = dfType <> IN
    val y = dfType <> OUT
    y := x

  class IDTop(argTop: Bit <> CONST = 1) extends DFDesign:
    val x   = SInt(16) <> IN
    val y   = SInt(16) <> OUT
    val id1 = ID()
    val id2 = new ID(argTop)
    id1.x <> x
    id1.y <> id2.x
    id2.y <> y

  class IDTopVia(argTop: Bit <> CONST = 1) extends DFDesign:
    self =>
    val x     = SInt(16) <> IN
    val y     = SInt(16) <> OUT
    val id1_x = SInt(16) <> VAR
    val id1_y = SInt(16) <> VAR
    val id2_x = SInt(16) <> VAR
    val id2_y = SInt(16) <> VAR
    val id1 = new ID(argTop):
      this.x <> id1_x
      this.y <> id1_y
    val id2 = new ID():
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
      """|class ID(val arg: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Basic ID design through companion constructor") {
    val id = (ID()).getCodeString
    assertNoDiff(
      id,
      """|class ID(val arg: Bit <> CONST = 0) extends DFDesign:
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
      """|class ID(val arg: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop(val argTop: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id1 = ID(arg = 0)
         |  val id2 = ID(arg = argTop)
         |  id1.x <> x
         |  id2.x <> id1.y
         |  y <> id2.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Generic ID design hierarchy") {
    class IDTopGen extends DFDesign:
      val x               = SInt(16) <> IN
      val y               = SInt(16) <> OUT
      val w: Int <> CONST = 16
      val id1             = new IDGen(SInt(w))
      val id2             = new IDGen(SInt(w))
      id1.x <> x
      id1.y <> id2.x
      id2.y <> y

    val id = (new IDTopGen).getCodeString
    assertNoDiff(
      id,
      """|class IDGen(val w: Int <> CONST) extends DFDesign:
         |  val x = SInt(w) <> IN
         |  val y = SInt(w) <> OUT
         |  y := x
         |end IDGen
         |
         |class IDTopGen extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val w: Int <> CONST = 16
         |  val id1 = IDGen(w = w)
         |  val id2 = IDGen(w = w)
         |  id1.x <> x.resize(w)
         |  id2.x <> id1.y
         |  y.resize(w) <> id2.y
         |end IDTopGen""".stripMargin
    )
  }
  test("Via-connection ID design hierarchy") {
    val id = (new IDTopVia).getCodeString
    assertNoDiff(
      id,
      """|class ID(val arg: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTopVia(val argTop: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val id1_x = SInt(16) <> VAR
         |  val id1_y = SInt(16) <> VAR
         |  val id2_x = SInt(16) <> VAR
         |  val id2_y = SInt(16) <> VAR
         |  val id1 = new ID(arg = argTop):
         |    this.x <>/*<--*/ id1_x
         |    this.y <>/*-->*/ id1_y
         |  val id2 = new ID(arg = 1):
         |    this.x <>/*<--*/ id2_x
         |    this.y <>/*-->*/ id2_y
         |  id1_x <> x
         |  id2_x <> id1_y
         |  y <> id2_y
         |end IDTopVia
         |""".stripMargin
    )
  }
  test("Design names affect named DFHDL types") {
    class ID extends DFDesign:
      case class ID() extends Opaque(Bit)
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = ID       <> VAR init 0.as(ID)
      y := x
    val id = (new ID).getCodeString
    assertNoDiff(
      id,
      """|class ID extends DFDesign:
         |  case class ID_0() extends Opaque(Bit)
         |
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = ID_0 <> VAR init 0.as(ID_0)
         |  y := x
         |end ID
         |""".stripMargin
    )
  }

  test("RTDesign with class extension and parameters") {
    val gp: Bit <> CONST     = 1
    val i: SInt[16] <> CONST = 0
    val i2                   = i + 5
    class ID(dp: Bit <> CONST) extends RTDesign:
      val c: SInt[16] <> CONST = 3
      val x                    = SInt(16) <> IN init i2
      val y                    = SInt(16) <> OUT
      val flag                 = Bit      <> IN init dp || gp
      y := x.reg.reg(2, init = c - i) - x
    end ID
    class IDExt(dpNew: Bit <> CONST) extends ID(gp && dpNew):
      val z = Bit <> OUT
      z := dpNew

    val id = (new IDExt(gp)).getCodeString
    assertNoDiff(
      id,
      """|val gp: Bit <> CONST = 1
         |val i: SInt[16] <> CONST = sd"16'0"
         |val i2: SInt[16] <> CONST = i + sd"16'5"
         |class IDExt(
         |    val dp: Bit <> CONST = gp && gp,
         |    val dpNew: Bit <> CONST = gp
         |) extends RTDesign:
         |  val c: SInt[16] <> CONST = sd"16'3"
         |  val x = SInt(16) <> IN init i2
         |  val y = SInt(16) <> OUT
         |  val flag = Bit <> IN init dp || gp
         |  y := x.reg.reg(2, init = c - i) - x
         |  val z = Bit <> OUT
         |  z := dpNew
         |end IDExt
         |""".stripMargin
    )
  }

  test("DFInt32 parameter propagation") {
    class ID(val width: Int <> CONST) extends DFDesign:
      val x = SInt(width + 1) <> IN init 0
      val y = SInt(width + 2) <> OUT
      y := x

    val id = (new ID(8)).getCodeString
    assertNoDiff(
      id,
      """|class ID(val width: Int <> CONST = 8) extends DFDesign:
         |  val x = SInt(width + 1) <> IN init sd"${width + 1}'0"
         |  val y = SInt(width + 2) <> OUT
         |  y := x.resize(width + 2)
         |end ID
         |""".stripMargin
    )
  }

  // TODO: need to fix param propagation from internal design upwards
  // test("DFInt32 hierarchy parameter propagation") {
  //   class StaticY() extends DFDesign:
  //     val width: Int <> CONST = 8
  //     val y                   = SInt(width) <> OUT
  //     y := 11

  //   class Top() extends DFDesign:
  //     val y     = SInt(8) <> OUT
  //     val st    = StaticY()
  //     val styp1 = st.y + 1
  //     y := styp1

  //   val id = Top().getCodeString
  //   assertNoDiff(
  //     id,
  //     """|class ID(val width: Int <> CONST = 8) extends DFDesign:
  //        |  val x = SInt(width + 1) <> IN init sd"${width + 1}'0"
  //        |  val y = SInt(width + 2) <> OUT
  //        |  y := x.resize(width + 2)
  //        |end ID
  //        |""".stripMargin
  //   )
  // }

  test("Basic EDDesign") {
    class ID extends EDDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val flag = Bit      <> IN
      val v    = SInt(16) <> VAR
      y <> x
      process:
        val z = SInt(8) <> VAR
        z := z + 1
        v :== 1
      process(x, y, flag.rising):
        val z = SInt(8) <> VAR
        z := 1
      process(all):
        val z = SInt(8) <> VAR
        object Bla:
          val z = Bits(8) <> VAR
          z := all(0)
        Bla.z
        z := 1
    end ID
    val id = (new ID).getCodeString(align = true)
    assertNoDiff(
      id,
      """|class ID extends EDDesign:
         |  val x     = SInt(16) <> IN
         |  val y     = SInt(16) <> OUT
         |  val flag  = Bit      <> IN
         |  val v     = SInt(16) <> VAR
         |  y     <>  x
         |  process:
         |    val z   = SInt(8)  <> VAR
         |    z   :=  z + sd"8'1"
         |    v   :== sd"16'1"
         |  process(x, y, flag.rising):
         |    val z   = SInt(8)  <> VAR
         |    z   :=  sd"8'1"
         |  process(all):
         |    val z_0 = SInt(8)  <> VAR
         |    val z_1 = Bits(8)  <> VAR
         |    z_1 :=  h"00"
         |    z_0 :=  sd"8'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Named anonymous multireference") {
    class IDMultiRef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT
      @inline def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + arg - constArg
      o := test(5)(data + 1)
    val id = (new IDMultiRef).getCodeString
    assertNoDiff(
      id,
      """|class IDMultiRef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val o_part = data + d"32'1"
         |  o := (o_part + o_part) - d"32'5"
         |end IDMultiRef
         |""".stripMargin
    )
  }
  test("Design def") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      /** This is my test
        * @param arg
        * @return
        */
      def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        test2(arg + arg) - constArg
      def test2(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg
      test(5)(data - 1)
      o := test(7)(data + 1)
      val x = test(10)(data)
      o := x
    end IDWithDesignDef
    val id = (new IDWithDesignDef).getCodeString
    assertNoDiff(
      id,
      """|def test2(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg
         |end test2
         |
         |/** This is my test
         |  * @param arg
         |  * @return
         |  **/
         |def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  test2(arg + arg) - constArg
         |end test
         |
         |class IDWithDesignDef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  test(constArg = d"32'5")(data - d"32'1")
         |  o := test(constArg = d"32'7")(data + d"32'1")
         |  val x = test(constArg = d"32'10")(data)
         |  o := x
         |end IDWithDesignDef
         |""".stripMargin
    )
  }
  test("Design def Unit return") {
    class UnitDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test2(arg: UInt[32] <> VAL): Unit <> DFRET =
        val x = arg + arg
      def test(arg: UInt[32] <> VAL): Unit <> DFRET =
        test2(arg)
      test(data)
      o := data
    val id = (new UnitDesignDef).getCodeString
    assertNoDiff(
      id,
      """|def test2(arg: UInt[32] <> VAL): Unit <> DFRET =
         |  val x = arg + arg
         |end test2
         |
         |def test(arg: UInt[32] <> VAL): Unit <> DFRET =
         |  test2(arg)
         |end test
         |
         |class UnitDesignDef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  test(data)
         |  o := data
         |end UnitDesignDef
         |""".stripMargin
    )
  }
  test("Design def with toScalaValue effects") {
    class DesignDefCont extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + const.toScalaInt
      o := test(1)(data)
      o := test(10)(data)
    val id = (new DesignDefCont).getCodeString
    assertNoDiff(
      id,
      """|def test_0(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test_0
         |
         |def test_1(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'10"
         |end test_1
         |
         |class DesignDefCont extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(const = d"8'1")(data)
         |  o := test_1(const = d"8'10")(data)
         |end DesignDefCont
         |""".stripMargin
    )
  }
  test("Named parameter should not be broken for IntP associative reductions") {
    // This tests checks for the issue that `maxCnt` is not reduced because of
    // -1 and +1 within the `UInt.to` which translates into `UInt(clog2(maxCnt+1))`
    class Blinker(
        val CLK_FREQ_KHz: Int <> CONST,
        val LED_FREQ_Hz: Int <> CONST
    ) extends RTDesign:
      /** Maximum half-count of the toggle for 50% duty cycle */
      val maxCnt = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2) - 1

      /** LED output */
      val led = Bit             <> OUT.REG init 1
      val cnt = UInt.to(maxCnt) <> VAR.REG init 0
      if (cnt == 5000000)
        cnt.din := 0
        led.din := !led
      else cnt.din := cnt + 1
    end Blinker
    val id = (new Blinker(50000, 1)).getCodeString
    assertNoDiff(
      id,
      """|class Blinker(
         |    val CLK_FREQ_KHz: Int <> CONST = 50000,
         |    val LED_FREQ_Hz: Int <> CONST = 1
         |) extends RTDesign:
         |  /** Maximum half-count of the toggle for 50% duty cycle */
         |  val maxCnt: Int <> CONST = ((CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2)) - 1
         |  /** LED output */
         |  val led = Bit <> OUT.REG init 1
         |  val cnt = UInt(clog2(maxCnt + 1)) <> VAR.REG init d"${clog2(maxCnt + 1)}'0"
         |  if (cnt == d"${clog2(maxCnt + 1)}'5000000")
         |    cnt.din := d"${clog2(maxCnt + 1)}'0"
         |    led.din := !led
         |  else cnt.din := cnt + d"${clog2(maxCnt + 1)}'1"
         |  end if
         |end Blinker
         |""".stripMargin
    )
  }
  test("Domains") {
    class IDWithDomains extends DFDesign:
      val y = SInt(16) <> OUT
      val fast = new RTDomain:
        val pr = SInt(16) <> VAR init 0
        val pw = SInt(16) <> VAR
        pr := pr.reg + 1
      val related = new fast.RelatedDomain:
        val x = SInt(16) <> VAR init 0
      y := fast.pr + related.x
      val fastdf = new DFDomain:
        val p = SInt(16) <> VAR
        p := 1
      fast.pw := fastdf.p
    end IDWithDomains
    val id = (new IDWithDomains).getCodeString
    assertNoDiff(
      id,
      """|class IDWithDomains extends DFDesign:
         |  val y = SInt(16) <> OUT
         |  val fast = new RTDomain:
         |    val pr = SInt(16) <> VAR init sd"16'0"
         |    val pw = SInt(16) <> VAR
         |    pr := pr.reg + sd"16'1"
         |  val related = new fast.RelatedDomain:
         |    val x = SInt(16) <> VAR init sd"16'0"
         |  y := fast.pr + related.x
         |  val fastdf = new DFDomain:
         |    val p = SInt(16) <> VAR
         |    p := sd"16'1"
         |  fast.pw := fastdf.p
         |end IDWithDomains
         |""".stripMargin
    )
  }
  test("Basic hierarchy with domains") {
    class IDTop extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val dmn1 = new RTDomain:
        val id = new ID(1)
        id.x <> x.reg(1, init = 0)
      val dmn2 = new RTDomain:
        val id = new ID(0)
        id.x <> dmn1.id.y
      y <> dmn2.id.y
    val top = (new IDTop).getCodeString
    assertNoDiff(
      top,
      """|class ID(val arg: Bit <> CONST = 1) extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  y := x
         |end ID
         |
         |class IDTop extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val dmn1 = new RTDomain:
         |    val id = ID(arg = 1)
         |    id.x <> x.reg(1, init = sd"16'0")
         |  val dmn2 = new RTDomain:
         |    val id = ID(arg = 0)
         |    id.x <> dmn1.id.y
         |  y <> id.y
         |end IDTop
         |""".stripMargin
    )
  }
  test("Docstrings"):
    /** HasDocs has docs */
    class HasDocs extends DFDesign:
      /** My in */
      val x = Bit <> IN

      /** My Out
        */
      val y = Bit <> OUT

      /** My very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very long doc
        */
      val z = Bit <> VAR
      z <> x
      y <> z
    end HasDocs

    val top = (new HasDocs).getCodeString
    assertNoDiff(
      top,
      """|/** HasDocs has docs */
         |class HasDocs extends DFDesign:
         |  /** My in */
         |  val x = Bit <> IN
         |  /** My Out
         |    **/
         |  val y = Bit <> OUT
         |  /** My very very very very very very very very very very very very very very very very very
         |    * very very very very very very very very very very very very very very very very very very
         |    * very very very very very very very very very very long doc
         |    **/
         |  val z = Bit <> VAR
         |  z <> x
         |  y <> z
         |end HasDocs
         |""".stripMargin
    )

  test("Blinker example"):

    /** This is a led blinker */
    class Blinker(
        val CLK_FREQ_KHz: Int <> CONST,
        val LED_FREQ_Hz: Int <> CONST
    ) extends RTDesign:
      /** Half-count of the toggle for 50% duty cycle */
      val HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2)

      /** LED output */
      val led = Bit                     <> OUT.REG init 1
      val cnt = UInt.until(HALF_PERIOD) <> VAR.REG init 0
      if (cnt == HALF_PERIOD - 1)
        cnt.din := 0
        led.din := !led
      else cnt.din := cnt + 1
    end Blinker
    val top = (Blinker(50000, 1)).getCodeString
    assertNoDiff(
      top,
      """|/** This is a led blinker */
         |class Blinker(
         |    val CLK_FREQ_KHz: Int <> CONST = 50000,
         |    val LED_FREQ_Hz: Int <> CONST = 1
         |) extends RTDesign:
         |  /** Half-count of the toggle for 50% duty cycle */
         |  val HALF_PERIOD: Int <> CONST = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2)
         |  /** LED output */
         |  val led = Bit <> OUT.REG init 1
         |  val cnt = UInt(clog2(HALF_PERIOD)) <> VAR.REG init d"${clog2(HALF_PERIOD)}'0"
         |  if (cnt == d"${clog2(HALF_PERIOD)}'${(HALF_PERIOD - 1)}")
         |    cnt.din := d"${clog2(HALF_PERIOD)}'0"
         |    led.din := !led
         |  else cnt.din := cnt + d"${clog2(HALF_PERIOD)}'1"
         |  end if
         |end Blinker
         |""".stripMargin
    )

  class BigXor(values: Vector[Bits[Int] <> CONST]) extends DFDesign:
    val sum = values.head.dfType <> OUT
    sum := values.reduce(_ ^ _)
  test("Unreachable anonymous global values"):
    val top = BigXor(Vector.tabulate(8)(i => h"4'$i")).getCodeString
    assertNoDiff(
      top,
      """|class BigXor extends DFDesign:
         |  val sum = Bits(4) <> OUT
         |  sum := ((((((h"0" ^ h"1") ^ h"2") ^ h"3") ^ h"4") ^ h"5") ^ h"6") ^ h"7"
         |end BigXor
         |""".stripMargin
    )
  test("Unreachable local values"):
    class BigXorContainer extends DFDesign:
      val sum = Bits(4) <> OUT
      val c   = h"4'7"
      val bx  = BigXor(Vector.tabulate(8)(i => c | h"4'$i"))
      sum <> bx.sum
    val top = BigXorContainer().getCodeString
    assertNoDiff(
      top,
      """|class BigXor(val c: Bits[4] <> CONST) extends DFDesign:
         |  val sum = Bits(4) <> OUT
         |  sum := (((((((c | h"0") ^ (c | h"1")) ^ (c | h"2")) ^ (c | h"3")) ^ (c | h"4")) ^ (c | h"5")) ^ (c | h"6")) ^ (c | h"7")
         |end BigXor
         |
         |class BigXorContainer extends DFDesign:
         |  val sum = Bits(4) <> OUT
         |  val c: Bits[4] <> CONST = h"7"
         |  val bx = BigXor(c = c)
         |  sum <> bx.sum
         |end BigXorContainer
         |""".stripMargin
    )
  // TODO: Currently this fails. Should it be fixed, or should out of reach types be out of spec?
  // test("Unreachable local values & types"):
  //   class BigXorContainer extends DFDesign:
  //     val w: Int <> CONST = 4
  //     val sum             = Bits(w) <> OUT
  //     val c               = h"$w'7"
  //     val bx              = BigXor(Vector.tabulate(8)(i => c | h"$w'$i"))
  //     sum <> bx.sum
  //   val top = BigXorContainer().getCodeString
  //   assertNoDiff(
  //     top,
  //     """|class BigXor(val c: Bits[4] <> CONST) extends DFDesign:
  //        |  val sum = Bits(4) <> OUT
  //        |  sum := (((((((c | h"0") ^ (c | h"1")) ^ (c | h"2")) ^ (c | h"3")) ^ (c | h"4")) ^ (c | h"5")) ^ (c | h"6")) ^ (c | h"7")
  //        |end BigXor
  //        |
  //        |class BigXorContainer extends DFDesign:
  //        |  val w: Int <> CONST = 4
  //        |  val sum = Bits(w) <> OUT
  //        |  val c: Bits[w.type] <> CONST = h"${w}'7"
  //        |  val bx = BigXor(c = c)
  //        |  sum <> bx.sum
  //        |end BigXorContainer
  //        |""".stripMargin
  //   )
  test("Cover case where same declaration domains are missing names"):
    class IDWithDomains extends EDDesign:
      @hw.flattenMode.suffix("_")
      val a, b = new EDDomain:
        val x = Bit <> IN
        val y = Bit <> OUT
        y <> x
    val top = IDWithDomains().getCodeString
    assertNoDiff(
      top,
      """|class IDWithDomains extends EDDesign:
         |  @hw.flattenMode.suffix("_")
         |  val a = new EDDomain:
         |    val x = Bit <> IN
         |    val y = Bit <> OUT
         |    y <> x
         |  @hw.flattenMode.suffix("_")
         |  val b = new EDDomain:
         |    val x = Bit <> IN
         |    val y = Bit <> OUT
         |    y <> x
         |end IDWithDomains
         |""".stripMargin
    )
  test("EDTrueDPR printing"):
    class TrueDPR(
        val DATA_WIDTH: Int <> CONST = 4,
        val ADDR_WIDTH: Int <> CONST = 4
    ) extends EDDesign:
      val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED

      val a, b = new EDDomain:
        val clk  = Bit              <> IN
        val data = Bits(DATA_WIDTH) <> IN
        val addr = Bits(ADDR_WIDTH) <> IN
        val q    = Bits(DATA_WIDTH) <> OUT
        val we   = Bit              <> IN

        process(clk):
          if (clk.rising)
            if (we)
              ram(addr) := data
            q :== ram(addr)
    end TrueDPR
    val top = TrueDPR().getCodeString
    assertNoDiff(
      top,
      """|class TrueDPR(
         |    val DATA_WIDTH: Int <> CONST = 4,
         |    val ADDR_WIDTH: Int <> CONST = 4
         |) extends EDDesign:
         |  val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED
         |  val a = new EDDomain:
         |    val clk = Bit <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT
         |    val we = Bit <> IN
         |    process(clk):
         |      if (clk.rising)
         |        if (we) ram(addr.uint.toInt) := data
         |        q :== ram(addr.uint.toInt)
         |      end if
         |  val b = new EDDomain:
         |    val clk = Bit <> IN
         |    val data = Bits(DATA_WIDTH) <> IN
         |    val addr = Bits(ADDR_WIDTH) <> IN
         |    val q = Bits(DATA_WIDTH) <> OUT
         |    val we = Bit <> IN
         |    process(clk):
         |      if (clk.rising)
         |        if (we) ram(addr.uint.toInt) := data
         |        q :== ram(addr.uint.toInt)
         |      end if
         |end TrueDPR
         |""".stripMargin
    )
  test("Exported defs don't disrupt naming") {
    object MyApply:
      export dfhdl.apply
    import MyApply.apply
    class Exporting extends DFDesign:
      val i  = Bits(2) <> IN
      val o  = Bit     <> OUT
      val x0 = i(0)
      val x1 = i(1)
      o := x0 ^ x1
    val id = (new Exporting).getCodeString
    assertNoDiff(
      id,
      """|class Exporting extends DFDesign:
         |  val i = Bits(2) <> IN
         |  val o = Bit <> OUT
         |  val x0 = i(0)
         |  val x1 = i(1)
         |  o := x0 ^ x1
         |end Exporting
         |""".stripMargin
    )
  }
  test("Fixed precedence of connection RHS") {
    class Precedence extends DFDesign:
      val x1 = Bits(8) <> IN
      val y1 = Bits(8) <> OUT
      y1 <> x1 | x1 & x1
      val x2 = Bits(8) <> IN
      val y2 = Bits(8) <> OUT
      y2 <> x2 ^ x2 ^ x2
      val x3 = Bit <> IN
      val y3 = Bit <> OUT
      y3 <> x3 && x3 || x3
    val id = (new Precedence).getCodeString
    assertNoDiff(
      id,
      """|class Precedence extends DFDesign:
         |  val x1 = Bits(8) <> IN
         |  val y1 = Bits(8) <> OUT
         |  y1 <> (x1 | (x1 & x1))
         |  val x2 = Bits(8) <> IN
         |  val y2 = Bits(8) <> OUT
         |  y2 <> ((x2 ^ x2) ^ x2)
         |  val x3 = Bit <> IN
         |  val y3 = Bit <> OUT
         |  y3 <> ((x3 && x3) || x3)
         |end Precedence
         |""".stripMargin
    )
  }
  test("Boolean selection operation") {
    class SelOp extends DFDesign:
      val c                     = Boolean <> IN
      val x1                    = Bits(8) <> IN
      val x2                    = Bits(8) <> IN
      val y1                    = Bits(8) <> OUT
      val cp: Boolean <> CONST  = true
      val up1: UInt[8] <> CONST = 11
      val up2: UInt[8] <> CONST = 22
      val up3: UInt[8] <> CONST = cp.sel(up1, up2)
      y1 := c.sel(x1, x2)
      y1 := c.sel(x1, all(0))
      y1 := c.sel(all(0), x2)
    val id = (new SelOp).getCodeString
    assertNoDiff(
      id,
      """|class SelOp extends DFDesign:
         |  val c = Boolean <> IN
         |  val x1 = Bits(8) <> IN
         |  val x2 = Bits(8) <> IN
         |  val y1 = Bits(8) <> OUT
         |  val cp: Boolean <> CONST = true
         |  val up1: UInt[8] <> CONST = d"8'11"
         |  val up2: UInt[8] <> CONST = d"8'22"
         |  val up3: UInt[8] <> CONST = cp.sel(up1, up2)
         |  y1 := c.sel(x1, x2)
         |  y1 := c.sel(x1, h"00")
         |  y1 := c.sel(h"00", x2)
         |end SelOp
         |""".stripMargin
    )
  }
  test("HighZ assignment") {
    class HighZ extends RTDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      if (x.|) y := x
      else y     := NOTHING
    val top = (new HighZ).getCodeString
    assertNoDiff(
      top,
      """|class HighZ extends RTDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  if (x.|) y := x
         |  else y := NOTHING
         |end HighZ
         |""".stripMargin
    )
  }
  test("If printing with parametric width") {
    class IfWithParams(
        val width: Int <> CONST = 8
    ) extends DFDesign:
      val sel   = Bit         <> IN
      val iBits = Bits(width) <> IN
      val oBits = Bits(width) <> VAR
      oBits := (if (sel) iBits else all(0))
    val top = (new IfWithParams).getCodeString
    assertNoDiff(
      top,
      """|class IfWithParams(val width: Int <> CONST = 8) extends DFDesign:
         |  val sel = Bit <> IN
         |  val iBits = Bits(width) <> IN
         |  val oBits = Bits(width) <> VAR
         |  oBits := ((
         |    if (sel) iBits
         |    else b"0".repeat(width)
         |  ): Bits[width.type] <> VAL)
         |end IfWithParams""".stripMargin
    )
  }
  test("Match printing with parametric width") {
    class MatchWithParams(
        val width: Int <> CONST = 8
    ) extends DFDesign:
      val sel   = Bit         <> IN
      val iBits = Bits(width) <> IN
      val oBits = Bits(width) <> VAR
      oBits := sel match
        case 0 => all(0)
        case 1 => iBits
    end MatchWithParams
    val top = (new MatchWithParams).getCodeString
    assertNoDiff(
      top,
      """|class MatchWithParams(val width: Int <> CONST = 8) extends DFDesign:
         |  val sel = Bit <> IN
         |  val iBits = Bits(width) <> IN
         |  val oBits = Bits(width) <> VAR
         |  oBits := ((
         |    sel match
         |      case 0 => b"0".repeat(width)
         |      case 1 => iBits
         |    end match
         |  ): Bits[width.type] <> VAL)
         |end MatchWithParams""".stripMargin
    )
  }
  test("RTDesign process printing") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          y.din := 0
          if (x) S1 else ThisStep
        def S1: Step =
          y.din := 1
          if (x) NextStep else S0
        def S2: Step =
          y.din := 0
          if (x) S2 else FirstStep
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  process:
         |    def S0: Step =
         |      y.din := 0
         |      if (x) S1
         |      else ThisStep
         |    end S0
         |    def S1: Step =
         |      y.din := 1
         |      if (x) NextStep
         |      else S0
         |    end S1
         |    def S2: Step =
         |      y.din := 0
         |      if (x) S2
         |      else FirstStep
         |    end S2
         |end Foo""".stripMargin
    )
  }
  test("RTDesign process steps printing with onEntry and onExit") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT
      process:
        def S_1: Step =
          def onEntry =
            x := 1
          10.ms.wait
          if (i) S_2 else S_1
        def S_2: Step =
          def onEntry =
            x := 0
          def onExit =
            x := 1
          10.ms.wait
          if (!i) S_1 else S_2
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT
         |  process:
         |    def S_1: Step =
         |      def onEntry: Unit =
         |        x := 1
         |      end onEntry
         |      10.ms.wait
         |      if (i) S_2
         |      else S_1
         |    end S_1
         |    def S_2: Step =
         |      def onEntry: Unit =
         |        x := 0
         |      end onEntry
         |      def onExit: Unit =
         |        x := 1
         |      end onExit
         |      10.ms.wait
         |      if (!i) S_1
         |      else S_2
         |    end S_2
         |end Foo""".stripMargin
    )
  }
  test("wait statements") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      val i = Bit <> IN
      process:
        x :== 1
        waitWhile(i)
        50.ms.wait
        x :== 0
        waitUntil(i.rising)
        50.us.wait
        x :== 1
        waitUntil(i)
        50.ns.wait
        x :== 0
        1.ns.wait
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends EDDesign:
         |  val x = Bit <> OUT
         |  val i = Bit <> IN
         |  process:
         |    x :== 1
         |    waitWhile(i)
         |    50.ms.wait
         |    x :== 0
         |    waitUntil(i.rising)
         |    50.us.wait
         |    x :== 1
         |    waitUntil(i)
         |    50.ns.wait
         |    x :== 0
         |    1.ns.wait
         |end Foo""".stripMargin
    )
  }
  test("for loop printing") {
    class Foo extends EDDesign:
      val matrix = Bits(10) X 8 X 8 <> OUT
      process:
        for (
          i <- 0 until 8;
          if i % 2 == 0;
          j <- 0 until 8;
          if j % 2 == 0;
          k <- 0 until 10
          if k % 2 == 0
        ) matrix(i)(j)(k) :== 1
        for (
          i <- 0 until 8;
          if i % 2 == 1;
          j <- 0 until 8;
          if j % 2 == 1;
          k <- 0 until 10
          if k % 2 == 1
        ) matrix(i)(j)(k) :== 0
        10.ns.wait
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends EDDesign:
         |  val matrix = Bits(10) X 8 X 8 <> OUT
         |  process:
         |    for (i <- 0 until 8)
         |      if ((i % 2) == 0)
         |        for (j <- 0 until 8)
         |          if ((j % 2) == 0)
         |            for (k <- 0 until 10)
         |              if ((k % 2) == 0) matrix(i)(j)(k) :== 1
         |            end for
         |          end if
         |        end for
         |      end if
         |    end for
         |    for (i <- 0 until 8)
         |      if ((i % 2) == 1)
         |        for (j <- 0 until 8)
         |          if ((j % 2) == 1)
         |            for (k <- 0 until 10)
         |              if ((k % 2) == 1) matrix(i)(j)(k) :== 0
         |            end for
         |          end if
         |        end for
         |      end if
         |    end for
         |    10.ns.wait
         |end Foo""".stripMargin
    )
  }
  test("for/while loop printing with COMB_LOOP") {
    class Foo extends RTDesign:
      val matrix = Bits(10) X 8 X 8 <> OUT.REG
      process:
        for (
          i <- 0 until 8;
          if i % 2 == 0;
          j <- 0 until 8;
          if j % 2 == 0;
          k <- 0 until 10
          if k % 2 == 0
        )
          COMB_LOOP
          matrix(i)(j)(k).din := 1
        val ii = UInt.until(8) <> VAR init 0
        while (ii != 7)
          COMB_LOOP
          matrix(ii)(0)(0).din := 0
          ii                   := ii + 1
        10.sec.wait
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends RTDesign:
         |  val matrix = Bits(10) X 8 X 8 <> OUT.REG
         |  process:
         |    for (i <- 0 until 8)
         |      COMB_LOOP
         |      if ((i % 2) == 0)
         |        for (j <- 0 until 8)
         |          COMB_LOOP
         |          if ((j % 2) == 0)
         |            for (k <- 0 until 10)
         |              COMB_LOOP
         |              if ((k % 2) == 0) matrix(i)(j)(k).din := 1
         |            end for
         |          end if
         |        end for
         |      end if
         |    end for
         |    val ii = UInt(3) <> VAR init d"3'0"
         |    while (ii != d"3'7")
         |      COMB_LOOP
         |      matrix(ii.toInt)(0)(0).din := 0
         |      ii := ii + d"3'1"
         |    end while
         |    10.sec.wait
         |end Foo""".stripMargin
    )
  }
  test("while loop printing") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      val b = Bit <> IN
      process:
        while (b)
          x :== b
          5.ns.wait
        while (true)
          x :== !b
          5.ns.wait
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo extends EDDesign:
         |  val x = Bit <> OUT
         |  val b = Bit <> IN
         |  process:
         |    while (b)
         |      x :== b
         |      5.ns.wait
         |    end while
         |    while (true)
         |      x :== !b
         |      5.ns.wait
         |    end while
         |end Foo""".stripMargin
    )
  }
  test("text out printing") {
    class Foo(val param: String <> CONST = "Hello\n..\"World\"!") extends EDDesign:
      val bar                      = param + "!"
      val param2                   = param + param
      val param3: Int <> CONST     = 42
      val param4                   = d"22"
      val param5                   = h"abc123"
      val param6                   = b"101010"
      val param7                   = d"-11"
      val param8: Bit <> CONST     = 1
      val param9: Boolean <> CONST = false
      enum MyEnum extends Encoded:
        case A, B, C
      val param10: MyEnum <> CONST = MyEnum.A

      process(all):
        assert(param == "hello2")
        report(param, Severity.Warning)
        assert(param == "hello2", s"I am the one ${param} who knocks")
        assert(param8, s"I\\am\nthe \"one\"(!)\n${param}\nwho\nknocks", Severity.Fatal)
        println(bar)
        println()
        print(s"I am the one ${param2} who knocks")
        print("hello")
        println(
          s"These are the values: $param3, $param4, $param5, $param6, $param7, $param8, $param9, $param10"
        )
        debug(param3, param4, param5, param6, param7, param8, param9, param10)
        finish()
    end Foo
    val top = (new Foo).getCodeString
    assertNoDiff(
      top,
      """|class Foo(val param: String <> CONST = "Hello\n..\"World\"!") extends EDDesign:
         |  enum MyEnum(val value: UInt[2] <> CONST) extends Encoded.Manual(2):
         |    case A extends MyEnum(d"2'0")
         |    case B extends MyEnum(d"2'1")
         |    case C extends MyEnum(d"2'2")
         |
         |  val bar: String <> CONST = param + "!"
         |  val param2: String <> CONST = param + param
         |  val param3: Int <> CONST = 42
         |  val param4: UInt[5] <> CONST = d"5'22"
         |  val param5: Bits[24] <> CONST = h"abc123"
         |  val param6: Bits[6] <> CONST = h"6'2a"
         |  val param7: SInt[5] <> CONST = sd"5'-11"
         |  val param8: Bit <> CONST = 1
         |  val param9: Boolean <> CONST = false
         |  val param10: MyEnum <> CONST = MyEnum.A
         |  process(all):
         |    assert(param == "hello2")
         |    report(s"${param}", Severity.Warning)
         |    assert(param == "hello2", s"I am the one ${param} who knocks")
         |    assert(param8, s"I\\am\nthe \"one\"(!)\n${param}\nwho\nknocks", Severity.Fatal)
         |    println(s"${bar}")
         |    println()
         |    print(s"I am the one ${param2} who knocks")
         |    print(s"hello")
         |    println(s"These are the values: ${param3}, ${param4}, ${param5}, ${param6}, ${param7}, ${param8}, ${param9}, ${param10}")
         |    debug(param3, param4, param5, param6, param7, param8, param9, param10)
         |    finish()
         |end Foo""".stripMargin
    )
  }
end PrintCodeStringSpec
