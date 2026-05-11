package CoreSpec
import dfhdl.*
import dfhdl.core.DFInt32
import munit.*

class DFDecimalSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLErrorLog(
      "Unsigned value width must be positive, but found: 0"
    )(
      """UInt(0)"""
    ) {
      UInt(zero)
    }
    val one = 1
    assertDSLErrorLog(
      "Signed value width must be larger than 1, but found: 1"
    )(
      """SInt(1)"""
    ) {
      SInt(one)
    }
    assertCodeString {
      """val x = UInt(8) <> VAR init d"8'0"
        |val y = SInt(8) <> VAR init sd"8'-1"
        |val z = SInt(8) <> VAR init sd"8'0"
        |""".stripMargin
    } {
      val x = UInt(8) <> VAR init 0
      val y = SInt(8) <> VAR init -1
      val z = SInt(8) <> VAR init 0
    }
  }
  def foo: Int <> DFRET = 1
  val u7 = UInt(7)
  val s5 = SInt(5)
  val until8 = UInt.until(8)
  val until9 = UInt.until(9)
  val max7 = UInt.to(7)
  val max8 = UInt.to(8)
  val s_until8 = SInt.untilAbs(8)
  val s_until9 = SInt.untilAbs(9)
  val s_max7 = SInt.toAbs(7)
  val s_max8 = SInt.toAbs(8)
  test("Inlined width") {
    u7.verifyWidth(7)
    s5.verifyWidth(5)
    until8.verifyWidth(3)
    until9.verifyWidth(4)
    max7.verifyWidth(3)
    max8.verifyWidth(4)
    s_until8.verifyWidth(4)
    s_until9.verifyWidth(5)
    s_max7.verifyWidth(4)
    s_max8.verifyWidth(5)
  }
  test("DFVal Conversion") {
    assertCodeString {
      """|val t0: Bits[6] <> CONST = h"6'00"
         |val t1: UInt[8] <> CONST = t0.uint.resize(8)
         |val t2 = UInt(8) <> VAR
         |val t3: UInt[6] <> CONST = t0.uint
         |val t4: SInt[7] <> CONST = t0.uint.signed
         |val t5: SInt[64] <> CONST = sd"64'0"
         |val t6: Bits[64] <> CONST = (t5 >> t0.uint.toInt).bits | (t5 << t0.uint.toInt).bits
         |t2 := t1
         |""".stripMargin
    } {
      val t0: Bits[6] <> CONST = all(0)
      val t1: UInt[8] <> CONST = t0
      val t2 = UInt(8) <> VAR
      val t3: UInt[Int] <> CONST = t0
      val t4: SInt[Int] <> CONST = t0
      val t5: SInt[64] <> CONST = 0
      val t6: Bits[64] <> CONST = (t5 >> t0).bits | (t5 << t0).bits
      t2 := t1
    }
  }
  test("Assignment") {
    assertCodeString {
      """|val c: UInt[8] <> CONST = d"8'1"
         |val i: Int <> CONST = 100
         |val ni: Int <> CONST = -100
         |val param: Int <> CONST = 8
         |val v = UInt(8) <> VAR init c + d"8'1"
         |val b8 = Bits(8) <> VAR
         |val u8 = UInt(8) <> VAR init d"8'255"
         |val s8 = SInt(8) <> VAR init ?
         |val u8b = UInt(8) <> VAR init d"8'${i}"
         |val u6 = UInt(6) <> OUT
         |val s6 = SInt(6) <> OUT
         |val u8p = UInt(param) <> VAR init d"${param}'0"
         |val s8p = SInt(param) <> VAR init sd"${param}'-1"
         |val ui = UInt(clog2(i)) <> VAR init d"${clog2(i)}'${(i - 1)}"
         |val si = SInt(8) <> VAR init sd"8'${ni}"
         |u8 := d"8'${i}"
         |val b6: Bits[6] <> CONST = h"6'00"
         |val s32: Int <> CONST = -120
         |val s32b = Int <> VAR
         |s32b := i
         |val s64 = SInt(64) <> VAR init sd"64'0"
         |val cu: UInt[1] <> CONST = d"1'1"
         |val cs: SInt[2] <> CONST = sd"2'-1"
         |u8 := d"8'0"
         |u8 := d"8'255"
         |u8 := d"8'0"
         |u8 := ?
         |u8 := d"8'7"
         |u8 := b6.uint.resize(8)
         |u8 := u6.resize(8)
         |s8 := (-u6.signed).resize(8)
         |s8 := -s8
         |s8 := sd"8'0"
         |s8 := sd"8'127"
         |s8 := sd"8'0"
         |s8 := ?
         |s8 := sd"8'-1"
         |s8 := sd"8'-127"
         |s8 := u6.signed.resize(8)
         |s8 := s6.resize(8)
         |u6 := u8.resize(6)
         |s6 := s8.resize(6)
         |val sl: Int <> CONST = 1 << param
         |val sr: Int <> CONST = 1 >> param
         |val pow: Int <> CONST = 2 ** param
         |val u4 = u6(3, 0)
         |val ubit = u6(1)
         |val s4 = s6(3, 0)
         |val sbit = s6(1)
         |val uint42: UInt[6] <> CONST = d"6'42"
         |val sint42: SInt[7] <> CONST = sd"7'42"
         |val sintNeg42: SInt[7] <> CONST = sd"7'-42"
         |val negSintNeg42: SInt[7] <> CONST = sd"7'42"
         |val absTest = abs(s4)
         |val absTest2: SInt[7] <> CONST = abs(sintNeg42)
         |val numSignedBit: Bit <> CONST = sint42(6)
         |val numSignedBitP = s8p(param - 1)
         |val ifTest =
         |  d"8'20" + ((
         |    if (u8 > d"8'5") u8
         |    else d"8'22"
         |  ): UInt[8] <> VAL)
         |""".stripMargin
    } {
      val c: UInt[8] <> CONST = 1
      val i: Int <> CONST = 100
      val ni: Int <> CONST = -100
      val param: Int <> CONST = 8
      assert(c.toScalaInt == 1)
      val v = UInt(8) <> VAR init c + 1
      assertCompileError(
        "Init value must be a constant."
      )(
        """val v2 = UInt(8) <> VAR init v + 1"""
      )
      val b8 = Bits(8) <> VAR
      val u8 = UInt(8) <> VAR init 255
      val s8 = SInt(8) <> VAR init ?
      val u8b = UInt(8) <> VAR init i
      val u6 = UInt(6) <> OUT
      val s6 = SInt(6) <> OUT
      val u8p = UInt[param.type] <> VAR init 0
      val s8p = SInt[param.type] <> VAR init -1
      val ui = UInt.until(i) <> VAR init d"${clog2(i)}'${i - 1}"
      val si = SInt(8) <> VAR init sd"8'${ni}"
      u8 := i
      assertDSLErrorLog(
        """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
           |An explicit conversion must be applied.""".stripMargin
      )("") {
        u8 := ni
      }
      assertDSLErrorLog(
        "The applied RHS value width (8) is larger than the LHS variable width (6)."
      )("") {
        s6 := ni
      }
      val b6: Bits[6] <> CONST = all(0)
      val s32: Int <> VAL = -120
      assertRuntimeErrorLog(
        """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
           |An explicit conversion must be applied.""".stripMargin
      ) {
        u8 := s32
      }
      assertRuntimeErrorLog(
        "The applied RHS value width (8) is larger than the LHS variable width (6)."
      ) {
        s6 := s32
      }
      val s32b = Int <> VAR
      s32b := i
      val s64: Long <> VAL = Long <> VAR init 0
      val cu: UInt[Int] <> VAL = 1
      val cs: SInt[Int] <> VAL = -1
      u8 := 0
      u8 := 255
      u8 := d"0"
      u8 := ?
      u8 := b"111"
      u8 := b6
      u8 := u6
      s8 := -u6
      s8 := -s8
      s8 := 0
      s8 := 127
      s8 := d"0"
      s8 := ?
      s8 := -1
      s8 := -127
      s8 := u6
      s8 := s6
      u6 := u8.truncate
      s6 := s8.truncate
      assertDSLErrorLog(
        """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
           |An explicit conversion must be applied.""".stripMargin
      )(
        """u8 := -1"""
      ) {
        val value = -1
        u8 := value
      }
      assertCompileError(
        """|Cannot apply a signed value to a bits variable.
           |Consider applying `.bits` conversion to resolve this issue.
           |""".stripMargin
      )(
        """b8 := s8"""
      )
      assertCompileError(
        """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
           |An explicit conversion must be applied.""".stripMargin
      )(
        """u8 := s8"""
      )
      assertCompileError(
        """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
           |An explicit conversion must be applied.""".stripMargin
      )(
        """u8 := s8"""
      )
      assertDSLErrorLog(
        "The applied RHS value width (9) is larger than the LHS variable width (8)."
      )(
        """u8 := 256"""
      ) {
        val value = 256
        u8 := value
      }
      assertDSLErrorLog(
        "The applied RHS value width (9) is larger than the LHS variable width (8)."
      )(
        """s8 := 128"""
      ) {
        val value = 128
        s8 := value
      }
      assertCompileError(
        "The applied RHS value width (9) is larger than the LHS variable width (8)."
      )(
        """s8 := u8"""
      )
      assertCompileError(
        "The applied RHS value width (9) is larger than the LHS variable width (8)."
      )(
        """s8 := -u8"""
      )
      val sl = 1 << param
      val sr = 1 >> param
      val pow = 2 ** param
      val u4 = u6(3, 0)
      val ubit = u6(1)
      val s4 = s6(3, 0)
      val sbit = s6(1)
      val str42 = "42"
      val strNeg42 = "-42"
      val uint42 = d"${str42}"
      val sint42 = sd"${str42}"
      val sintNeg42 = sd"${strNeg42}"
      val negSintNeg42 = -sd"${strNeg42}"
      val absTest = abs(s4)
      val absTest2: SInt[7] <> CONST = abs(sintNeg42)
      val numSignedBit = sint42.signbit
      val numSignedBitP = s8p.signbit
      assertRuntimeErrorLog(
        """|Unexpected negative value found for unsigned decimal string interpolation: -42
           |To Fix: Use the signed decimal string interpolator `sd` instead.""".stripMargin
      ) {
        d"${strNeg42}"
      }
      val ifTest = d"8'20" + (if (u8 > 5) u8 else 22)
    }
    assertDSLErrorLog(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.""".stripMargin
    )(
      """val cu: UInt[Int] <> VAL = -1"""
    ) {
      val value = -1
      val cu: UInt[Int] <> VAL = value
    }
  }
  test("Comparison") {
    val u8 = UInt(8) <> VAR
    val u7 = UInt(7) <> VAR
    val s8 = SInt(8) <> VAR
    val b8 = Bits(8) <> VAR
    val one: Int <> CONST = 1
    val negOne: Int <> CONST = -1
    val big: Int <> CONST = 1000
    assertCodeString {
      """|val t1 = u8 == u8
         |val t2 = u8 != d"8'0"
         |val t3 = d"8'0" < u8
         |val t4 = u8 > d"8'12"
         |val t5 = u8 != d"8'255"
         |val t6 = u8 <= b8.uint
         |val t7 = u8.resize(4) >= b8.resize(4).uint
         |val t8 = u8 == d"8'${one}"
         |val t9 = s8 == sd"8'${one}"
         |val t10 = s8 == sd"8'${negOne}"
         |val t11 = u8 < d"8'${one}"
         |val t12 = s8 >= sd"8'${one}"
         |val t13 = s8 <= sd"8'${negOne}"
         |""".stripMargin
    } {
      val t1 = u8 == u8
      val t2 = u8 != 0
      val t3 = 0 < u8
      val t4 = u8 > d"8'12"
      val t5 = u8 != h"FF"
      val t6 = u8 <= b8
      val t7 = u8.resize(4) >= b8.resize(4)
      val t8 = u8 == one
      val t9 = s8 == one
      val t10 = s8 == negOne
      val t11 = u8 < one
      val t12 = s8 >= one
      val t13 = s8 <= negOne
    }

    assertDSLErrorLog(
      """|Cannot apply this operation between a value of 8 bits width (LHS) to a value of 7 bits width (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """u8 == u7"""
    ) {
      val value = 7
      val u7 = UInt(value) <> VAR
      u8 == u7
    }
    assertDSLErrorLog(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """u8 < -1"""
    ) {
      val value = -1
      u8 < value
    }
    assertDSLErrorLog(
      """|Cannot apply this operation between a signed value (LHS) and an unsigned value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """-1 <= u8"""
    ) {
      val value = -1
      value <= u8
    }
    assertDSLErrorLog(
      "The wildcard `Int` value width (10) is larger than the bit-accurate value width (8)."
    )(
      """u8 > 1000"""
    ) {
      val value = 1000
      u8 > value
    }
    assertRuntimeErrorLog(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    ) {
      u8 == negOne
    }
    assertRuntimeErrorLog(
      "The wildcard `Int` value width (10) is larger than the bit-accurate value width (8)."
    ) {
      u8 == big
    }
    assertRuntimeErrorLog(
      "The wildcard `Int` value width (11) is larger than the bit-accurate value width (8)."
    ) {
      s8 == big
    }
  }
  test("Arithmetic") {
    assertEquals(d"8'22" + d"8'22", d"8'44")
    assertEquals(d"8'22" +^ d"8'22", d"9'44")
    assertEquals(d"5'22" +^ d"8'22", d"9'44")
    assertEquals(d"8'22" +^ d"5'22", d"9'44")
    assertEquals(sd"8'22" + sd"8'22", sd"8'44")
    assertEquals(sd"8'22" + 22, sd"8'44")
    assertEquals(d"8'22" + h"4", d"8'26")
    assertEquals(d"8'255" + d"8'1", d"8'0")
    assertEquals(d"8'255" +^ d"8'1", d"9'256")
    assertEquals(sd"9'255" + sd"8'1", sd"9'-256")
    assertEquals(sd"9'255" +^ sd"8'1", sd"10'256")
    assertEquals(200 + d"8'1", d"8'201")
    assertEquals(200 +^ d"8'1", d"9'201")
    // -200 is a wildcard that adapts to SInt[8], but -200 doesn't fit → error
    // assertEquals(-200 + sd"8'1", sd"9'-199") // now an error with wildcard rules
    assertEquals(-100 + sd"8'1", sd"8'-99")
    // Commutative + allows mixed width and signedness (DFVal + DFVal)
    assertEquals(sd"8'22" + d"8'22", sd"9'44")
    assertEquals(d"8'22" + sd"8'22", sd"9'44")
    assertEquals(h"8'22" + sd"8'22", sd"9'56")
    assertEquals(d"8'22" + d"9'22", d"9'44")
    // Wildcard Int literals adapt to bit-accurate value
    assertEquals(d"8'22" + 200, d"8'222")
    assertEquals(sd"8'-1" + 1, sd"8'0")
    assertEquals(22 + sd"8'22", sd"8'44")
    assertEquals(sd"8'22" + 100, sd"8'122")
    // These are now errors with wildcard rules (literal doesn't fit bit-accurate value):
    // assertEquals(d"8'22" + (-22), sd"9'0")   // -22 negative for UInt
    // assertEquals(sd"8'22" + 200, sd"9'222")  // 200 exceeds SInt[8]
    assertEquals(d"8'22" - d"8'22", d"8'0")
    assertEquals(sd"8'22" - sd"8'22", sd"8'0")
    assertEquals(sd"8'22" - 22, sd"8'0")
    assertEquals(d"8'22" - b"1001", d"8'13")
    assertEquals(d"8'22" - d"8'23", d"8'255")
    assertEquals(sd"8'22" - sd"8'23", sd"8'-1")
    assertCompileError(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """sd"8'22" - d"8'22""""
    )
    assertCompileError(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """sd"8'22" - h"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """d"8'22" - sd"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """d"8'22" - (-22)"""
    )
    assertCompileError(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """d"8'22" - d"9'22""""
    )
    assertEquals(d"8'22" - 200, d"8'78")
    assertEquals(sd"9'22" - 200, sd"9'-178")
    assertDSLErrorLog(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """sd"8'22" - 200"""
    ) {
      val value = 200
      sd"8'22" - value
    }
    // Wildcard adapts: 22 adapts to SInt[8] bit-accurate value
    val subWild = 22 - sd"8'22"

    assertEquals(d"8'22" * d"8'2", d"8'44")
    assertEquals(d"8'22" / d"8'2", d"8'11")
    assertEquals(d"8'22" % d"8'2", d"8'0")
    assertEquals(100 * d"7'2", d"7'72")
    assertEquals(100 / d"7'2", d"7'50")
    // 5 % d"3'2": wildcard 5 adapts to UInt[3], 5 % 2 = 1
    assertEquals(5 % d"3'2", d"3'1")
    assertEquals(d"8'22" *^ d"8'2", d"16'44")
    assertEquals(100 *^ d"7'2", d"14'200")

    val u8 = UInt(8) <> VAR
    val u7 = UInt(7) <> VAR
    val s8 = SInt(8) <> VAR
    val b8 = Bits(8) <> VAR
    assertCodeString {
      """|val t1 = u8 + u8
         |val t2 = u8 - d"8'0"
         |val t3 = d"8'200" - u8
         |val t4 = s8 / sd"8'2"
         |val t5 = u8 % d"8'9"
         |val t6 = u8 * d"8'22"
         |val t7 = s8 + sd"8'22"
         |val t8 = s8 +^ sd"8'1"
         |val t9 = u8 -^ d"8'22"
         |val t10 = d"7'100" *^ u8
         |""".stripMargin
    } {
      val t1 = u8 + u8
      t1.verifyValOf[UInt[8]]
      val t2 = u8 - 0
      t2.verifyValOf[UInt[8]]
      val t3 = 200 - u8
      t3.verifyValOf[UInt[8]]
      val t4 = s8 / 2
      t4.verifyValOf[SInt[8]]
      val t5 = u8 % 9
      t5.verifyValOf[UInt[8]]
      val t6 = u8 * d"22"
      t6.verifyValOf[UInt[8]]
      val t7 = s8 + sd"22"
      t7.verifyValOf[SInt[8]]
      val t8 = s8 +^ 1
      t8.verifyValOf[SInt[9]]
      val t9 = u8 -^ d"22"
      t9.verifyValOf[UInt[9]]
      val t10 = 100 *^ u8
      t10.verifyValOf[UInt[15]]
    }
    // Commutative + and * allow mixed width/signedness
    val t11 = s8 + d"8'22"
    val t12 = b8 * s8
    // Non-commutative -, /, % keep strict constraints
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """u8 - sd"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """u8 / (-22)"""
    )
    assertCompileError(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """u8 % d"9'22""""
    )
  }
  test("Wildcard Int operands") {
    val u8 = UInt(8) <> VAR
    val s8 = SInt(8) <> VAR
    val b8 = Bits(8) <> VAR
    val param: Int <> CONST = 10
    val i42: Int = 42

    // === Commutative + with Scala Int literals ===
    val t1 = u8 + 5; t1.verifyValOf[UInt[8]]
    val t2 = 5 + u8; t2.verifyValOf[UInt[8]]
    val t3 = s8 + 5; t3.verifyValOf[SInt[8]]
    val t4 = 5 + s8; t4.verifyValOf[SInt[8]]
    val t5 = (-5) + s8; t5.verifyValOf[SInt[8]]
    val t6 = s8 + (-5); t6.verifyValOf[SInt[8]]

    // === Commutative * with Scala Int literals ===
    val t7 = u8 * 3; t7.verifyValOf[UInt[8]]
    val t8 = 3 * u8; t8.verifyValOf[UInt[8]]
    val t9 = s8 * 3; t9.verifyValOf[SInt[8]]
    val t10 = 3 * s8; t10.verifyValOf[SInt[8]]

    // === Commutative + with Scala non-literal Int ===
    val t11 = u8 + i42; t11.verifyValOf[UInt[8]]
    val t12 = i42 + u8; t12.verifyValOf[UInt[8]]
    val t13 = s8 + i42; t13.verifyValOf[SInt[8]]
    val t14 = i42 + s8; t14.verifyValOf[SInt[8]]

    // === Commutative * with Scala non-literal Int ===
    val t15 = u8 * i42; t15.verifyValOf[UInt[8]]
    val t16 = i42 * u8; t16.verifyValOf[UInt[8]]
    val t17 = s8 * i42; t17.verifyValOf[SInt[8]]
    val t18 = i42 * s8; t18.verifyValOf[SInt[8]]

    // === Commutative + with DFHDL Int param ===
    val t19 = u8 + param; t19.verifyValOf[UInt[8]]
    val t20 = param + u8; t20.verifyValOf[UInt[8]]
    val t21 = s8 + param; t21.verifyValOf[SInt[8]]
    val t22 = param + s8; t22.verifyValOf[SInt[8]]

    // === Commutative * with DFHDL Int param ===
    val t23 = u8 * param; t23.verifyValOf[UInt[8]]
    val t24 = param * u8; t24.verifyValOf[UInt[8]]
    val t25 = s8 * param; t25.verifyValOf[SInt[8]]
    val t26 = param * s8; t26.verifyValOf[SInt[8]]

    // === Bits (implicit UInt) with wildcards ===
    val t27 = b8 + 5
    val t28 = 5 + b8
    val t29 = b8 + param
    val t30 = param + b8

    // === Commutative max/min with wildcards ===
    val t27b = u8 max 5; t27b.verifyValOf[UInt[8]]
    val t27c = 5 max u8; t27c.verifyValOf[UInt[8]]
    val t27d = s8 max (-5); t27d.verifyValOf[SInt[8]]
    val t27e = (-5) max s8; t27e.verifyValOf[SInt[8]]
    val t27f = u8 min i42; t27f.verifyValOf[UInt[8]]
    val t27g = i42 min u8; t27g.verifyValOf[UInt[8]]
    val t27h = s8 min i42; t27h.verifyValOf[SInt[8]]
    val t27i = i42 min s8; t27i.verifyValOf[SInt[8]]
    val t27j = u8 max param; t27j.verifyValOf[UInt[8]]
    val t27k = param max u8; t27k.verifyValOf[UInt[8]]
    val t27l = s8 min param; t27l.verifyValOf[SInt[8]]
    val t27m = param min s8; t27m.verifyValOf[SInt[8]]

    // === Param + Param: stays Int (DFInt32) ===
    val param2: Int <> CONST = 20
    val t31 = param + param2; t31.verifyValOf[DFInt32]
    val t32 = param * param2; t32.verifyValOf[DFInt32]
    val t33b = param max param2; t33b.verifyValOf[DFInt32]
    val t33c = param min param2; t33c.verifyValOf[DFInt32]

    // === Non-commutative ops: wildcard adapts to LHS ===
    val t33 = u8 - 3; t33.verifyValOf[UInt[8]]
    val t34 = u8 - i42; t34.verifyValOf[UInt[8]]
    val t35 = u8 - param; t35.verifyValOf[UInt[8]]
    val t36 = u8 / 3; t36.verifyValOf[UInt[8]]
    val t37 = u8 / i42; t37.verifyValOf[UInt[8]]
    val t38 = u8 / param; t38.verifyValOf[UInt[8]]
    val t39 = s8 - 3; t39.verifyValOf[SInt[8]]
    val t40 = s8 - i42; t40.verifyValOf[SInt[8]]
    val t41 = s8 - param; t41.verifyValOf[SInt[8]]
    val t42 = s8 % 3; t42.verifyValOf[SInt[8]]
    val t43 = s8 % i42; t43.verifyValOf[SInt[8]]
    val t44 = s8 % param; t44.verifyValOf[SInt[8]]

    // === Non-commutative with wildcard LHS ===
    // Wildcard always adapts, even in non-commutative ops
    val t45 = 200 - u8; t45.verifyValOf[UInt[8]]
    val t46 = i42 - u8; t46.verifyValOf[UInt[8]]
    val t47 = param - u8; t47.verifyValOf[UInt[8]]
    val t48 = param / u8; t48.verifyValOf[UInt[8]]

    // === Constant propagation ===
    // All-constant expressions produce CONST results
    val c1: UInt[8] <> CONST = d"8'22" + 5
    val c2: UInt[8] <> CONST = 5 + d"8'22"
    val c3: SInt[8] <> CONST = sd"8'22" + (-5)
    val c4: UInt[8] <> CONST = d"8'22" * d"8'2"
    val c5: UInt[8] <> CONST = 3 * d"8'22"
    // Param + literal is also CONST (both are CONST)
    val c6: UInt[8] <> CONST = d"8'5" + param
    val c7: SInt[8] <> CONST = sd"8'5" + param
    // Param + VAR is NOT const (verified by type — no CONST annotation)
    val nc1 = u8 + param // result is not CONST
    val nc2 = param + u8 // result is not CONST

    // === Comparisons: wildcard adapts ===
    val cmp1 = u8 == 200
    // 200 == u8 not supported (Scala primitive LHS with ==)
    val cmp3 = s8 < (-5)
    val cmp4 = (-5) < s8
    val cmp5 = u8 == param
    val cmp6 = param == u8
    val cmp7 = s8 < param
    val cmp8 = param < s8
    val cmp9 = u8 == i42
    // i42 == u8 not supported (Scala primitive LHS with ==)
    val cmp11 = s8 < i42
    val cmp12 = i42 < s8

    // Compile-time errors for literal value-fit checking
    assertCompileError(
      "The wildcard `Int` value width (10) is larger than the bit-accurate value width (8)."
    )("""u8 + 1000""")
    assertCompileError(
      "Cannot apply a signed wildcard `Int` value to an unsigned bit-accurate value.\nUse an explicit conversion or `sd\"\"` interpolation."
    )("""u8 + (-1)""")
    assertCompileError(
      "The wildcard `Int` value width (11) is larger than the bit-accurate value width (8)."
    )("""s8 + 1000""")
    // Non-commutative: literal wildcard LHS that doesn't fit
    assertCompileError(
      "The wildcard `Int` value width (10) is larger than the bit-accurate value width (8)."
    )("""1000 - u8""")
    assertCompileError(
      "Cannot apply a signed wildcard `Int` value to an unsigned bit-accurate value.\nUse an explicit conversion or `sd\"\"` interpolation."
    )("""(-1) - u8""")
    // Unsigned wildcard adapting to signed bit-accurate value needs extra bit
    assertCompileError(
      "The wildcard `Int` value width (9) is larger than the bit-accurate value width (8)."
    )("""255 + s8""")
    assertCompileError(
      "The wildcard `Int` value width (9) is larger than the bit-accurate value width (8)."
    )("""s8 + 255""")
    assertCompileError(
      "The wildcard `Int` value width (9) is larger than the bit-accurate value width (8)."
    )("""255 - s8""")

    // Elaboration-time errors for non-literal value-fit checking
    assertDSLErrorLog(
      "Wildcard `Int` value width (10) is larger than the bit-accurate value width (8)."
    )(
      ""
    ) {
      val bigVal: Int <> CONST = 1000
      u8 + bigVal
    }
    assertDSLErrorLog(
      "Wildcard `Int` value is negative and cannot adapt to unsigned bit-accurate value UInt[8]."
    )(
      ""
    ) {
      val negVal: Int <> CONST = -1
      u8 + negVal
    }
    // Unsigned wildcard adapting to signed bit-accurate value at elaboration time
    assertDSLErrorLog(
      "Wildcard `Int` value width (9) is larger than the bit-accurate value width (8)."
    )(
      ""
    ) {
      val bigUnsigned: Int <> CONST = 255
      s8 + bigUnsigned
    }
  }
  test("d\"\" unsigned-only interpolation") {
    // d"" produces unsigned UInt constants
    assertEquals(d"0", d"1'0")
    assertEquals(d"255", d"8'255")
    assertEquals(d"8'42", d"8'42")

    // d"" rejects negative values at compile time
    assertCompileError(
      """Negative value in unsigned `d""` interpolation. Use `sd""` for signed values."""
    )(
      """d"-1""""
    )
    assertCompileError(
      """Negative value in unsigned `d""` interpolation. Use `sd""` for signed values."""
    )(
      """d"8'-1""""
    )

    // sd"" produces signed SInt constants (positive and negative)
    assertEquals(sd"0", sd"2'0")
    assertEquals(sd"-1", sd"2'-1")
    assertEquals(sd"8'42", sd"8'42")
    assertEquals(sd"8'-1", sd"8'-1")

    // d"" with runtime negative value is a runtime error
    assertRuntimeErrorLog(
      """|Unexpected negative value found for unsigned decimal string interpolation: -5
         |To Fix: Use the signed decimal string interpolator `sd` instead.""".stripMargin
    ) {
      val negVal = -5
      d"$negVal"
    }
  }
  test("Arithmetic position") {
    val u8 = UInt(8) <> VAR
    val b8 = Bits(8) <> VAR
    // single binary op: position spans the full expression
    val t1 = u8 + u8; t1.assertPosition(0, 1, 14, 21)
    // chained +: NOT merged (carry promotion requires binary), position is the last op
    val t2 = u8 + u8 + u8; t2.assertPosition(0, 1, 14, 26)
    // chained ^: merged into multi-arg, position spans from first to last operand
    val t3 = b8 ^ b8 ^ b8; t3.assertPosition(0, 1, 14, 26)
  }
  test("Arithmetic auto-carry promotion") {
    val u8 = UInt(8) <> VAR
    val u5 = UInt(5) <> VAR
    val s8 = SInt(8) <> VAR
    val u8b = UInt(8) <> VAR
    val u9 = UInt(9) <> VAR
    val u10 = UInt(10) <> VAR
    val u12 = UInt(12) <> VAR
    val u16 = UInt(16) <> VAR
    val s9 = SInt(9) <> VAR
    assertCodeString {
      """|u9 := u8 +^ u8
         |u9 := u8 -^ u8
         |u16 := u8 *^ u8
         |u10 := (u8 +^ u8).resize(10)
         |u8b := u8 + u8
         |val sum = u8 + u8
         |u9 := sum.resize(9)
         |s9 := s8 +^ s8
         |u9 := (u8 / u8).resize(9)
         |u9 := u8 +^ u5.resize(8)
         |u9 := u8 +^ d"8'200"
         |u12 := (u8 *^ u8).resize(12)
         |""".stripMargin
    } {
      // Basic carry promotion for +
      u9 := u8 + u8
      // Basic carry promotion for -
      u9 := u8 - u8
      // Basic carry promotion for *
      u16 := u8 * u8
      // Target wider than carry width: promote to 9, resize to 10
      u10 := u8 + u8
      // Target = func width: no promotion
      u8b := u8 + u8
      // Named value: no promotion
      val sum = u8 + u8
      u9 := sum
      // SInt version
      s9 := s8 + s8
      // Division: no carry variant, normal resize
      u9 := u8 / u8
      // Asymmetric widths: u8 + u5 → func width 8, carry = 9
      u9 := u8 + u5
      // Int literal: 200 is 8 bits, carry width = 9
      u9 := u8 + 200
      // Partial mul promotion: target (12) > funcWidth (8), promote to 16, resize to 12
      u12 := u8 * u8
    }
  }
  test("Int32 arithmetic") {
    val param: Int <> CONST = 2
    val t1 = 1 + param
    val r1: Int <> CONST = 3
    assertEquals(t1, r1)
    val t2 = 1 - param
    val r2: Int <> CONST = -1
    assertEquals(t2, r2)
    val t3 = 4 * param
    val r3: Int <> CONST = 8
    assertEquals(t3, r3)
    val t4 = 10 / param
    val r4: Int <> CONST = 5
    assertEquals(t4, r4)
    val t5 = 10 % param
    val r5: Int <> CONST = 0
    assertEquals(t5, r5)
    val t6 = 3 ** param
    val r6: Int <> CONST = 9
    assertEquals(t6, r6)
    val t7 = 1 max param
    val r7: Int <> CONST = 2
    assertEquals(t7, r7)
    val t8 = 1 min param
    val r8: Int <> CONST = 1
    assertEquals(t8, r8)
    val t9 = 2 ** param
    val r9: Int <> CONST = 4
    assertEquals(t9, r9)
  }
  test("Implicit Int Verilog-semantics warning") {
    val warnMsg =
      """|Implicit Scala/DFHDL Int conversion may produce different results than Verilog.
         |In Verilog, integer literals are 32-bit, which can widen intermediate arithmetic.
         |In DFHDL, Int literals are converted to minimum bit-accurate width.
         |Use carry operations (+^, -^, *^) or explicit bit-accurate literals (d"W'V").""".stripMargin
    val u8 = UInt(8) <> VAR
    val a = UInt(8) <> VAR
    val b = UInt(8) <> VAR
    val c = UInt(8) <> VAR
    val d = UInt(8) <> VAR

    // Should warn: (a + b + c + d) / 4
    assertRuntimeWarningLog(warnMsg) {
      val t1 = (a + b + c + d) / 4
    }

    // Should warn: (a * 3 + b) / 4
    assertRuntimeWarningLog(warnMsg) {
      val t2 = (a * 3 + b) / 4
    }

    // Should warn: (a + b) % 3
    assertRuntimeWarningLog(warnMsg) {
      val t2b = (a + b) % 3
    }

    // Should warn: (a + b + 0) >> 1 — "Forcing Larger Evaluation" Verilog pattern
    assertRuntimeWarningLog(warnMsg) {
      val t2c = (a + b + 0) >> 1
    }

    // Should NOT warn: (a + b) >> 2 — no implicit Int in the + chain
    val t2d = (a + b) >> 2

    // Should warn: wider target with implicit Int in chain
    val sum = UInt(10) <> VAR
    assertRuntimeWarningLog(warnMsg) {
      sum := a + b + c + d + 1
    }

    // Should NOT warn: wider target but explicit literal
    sum := a + b + c + d + d"1"

    // Should NOT warn: wider target but single op, carry promotion handles it
    sum := u8 + 1

    // Should warn: wider target with chain, intermediate overflow
    assertRuntimeWarningLog(warnMsg) {
      sum := u8 + u8 + 1
    }

    // Should NOT warn: target width == expression width
    u8 := u8 + 1

    // Should NOT warn: a / 4 (no anonymous arith chain)
    val t3 = a / 4

    // Should NOT warn: carry ops used
    val t4 = (a +^ b +^ c +^ d) / 4

    // Should NOT warn: explicit bit-accurate literal
    val t5 = (a + b + c + d) / d"3'4"

    // Should warn: DFHDL Int <> CONST used as divisor
    val p: Int <> CONST = 4
    assertRuntimeWarningLog(warnMsg) {
      val t6 = (a + b + c + d) / p
    }

    // Should NOT warn: operands are 32-bit or wider
    val w32 = UInt(32) <> VAR
    val w32b = UInt(32) <> VAR
    val t7 = (w32 + w32b) / 4

    assertNoWarnings()
  }
  val sint8 = SInt(8) <> VAR
  test("Clean type in error messages (no ExactOp2Aux leakage)") {
    val errors = compiletime.testing.typeCheckErrors("(sint8 + sint8).sint")
    assert(errors.nonEmpty, "Expected a compile error for .sint on SInt")
    val allMessages = errors.map(_.message).mkString("\n")
    assert(
      !allMessages.contains("ExactOp2Aux"),
      s"Error message should not contain ExactOp2Aux projection types:\n$allMessages"
    )
  }
end DFDecimalSpec
