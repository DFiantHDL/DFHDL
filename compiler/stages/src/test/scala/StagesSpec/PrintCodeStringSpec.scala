package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{getCodeString, sanityCheck}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintCodeStringSpec extends StageSpec:
  class ID(arg: Bit <> CONST = 1) extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y := x

  class IDGen[T <: DFType](dfType: T) extends DFDesign:
    val x = dfType <> IN
    val y = dfType <> OUT
    y := x

  class IDTop(argTop: Bit <> CONST = 1) extends DFDesign:
    val x   = SInt(16) <> IN
    val y   = SInt(16) <> OUT
    val id1 = ID(0)
    val id2 = ID(argTop)
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
      """|class ID(val arg: Bit <> CONST) extends DFDesign:
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
  test("Via-connection ID design hierarchy") {
    val id = (new IDTopVia).getCodeString
    assertNoDiff(
      id,
      """|class ID(val arg: Bit <> CONST) extends DFDesign:
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
      process.forever:
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
         |  process.forever:
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
         |    val pr = SInt(16) <> VAR init sd"16'0"
         |    val pw = SInt(16) <> VAR
         |    pr := pr.reg + sd"16'1"
         |  y := fast.pr
         |  val fastdf = new DFDomain:
         |    val p = SInt(16) <> VAR
         |    p := sd"16'1"
         |  fast.pw := fastdf.p
         |end IDWithDomains
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

end PrintCodeStringSpec
