package CoreSpec
import dfhdl.*
import munit.*

class DFDecimalSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLError(
      "Unsigned value width must be positive, but found: 0"
    )(
      """UInt(0)"""
    ) {
      UInt(zero)
    }
    val one = 1
    assertDSLError(
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
  val max7 = UInt.max(7)
  val max8 = UInt.max(8)
  test("Inlined width") {
    u7.width.verifyInlined(7)
    s5.width.verifyInlined(5)
    until8.width.verifyInlined(3)
    until9.width.verifyInlined(4)
    max7.width.verifyInlined(3)
    max8.width.verifyInlined(4)
  }
  test("DFVal Conversion") {
    assertCodeString {
      """|val t0: Bits[6] <> CONST = h"6'00"
         |val t1: UInt[8] <> CONST = t0.uint.resize(8)
         |val t2 = UInt(8) <> VAR
         |val t3: UInt[6] <> CONST = t0.uint
         |val t4: SInt[7] <> CONST = t0.uint.signed
         |t2 := t1
         |""".stripMargin
    } {
      val t0: Bits[6] <> CONST = all(0)
      val t1: UInt[8] <> CONST = t0
      val t2 = UInt(8) <> VAR
      val t3: UInt[Int] <> CONST = t0
      val t4: SInt[Int] <> CONST = t0
      t2 := t1
    }
  }
  test("Assignment") {
    assertCodeString {
      """|val c: UInt[8] <> CONST = d"8'1"
         |val i: Int <> CONST = 100
         |val ni: Int <> CONST = -100
         |val v = UInt(8) <> VAR init c + d"8'1"
         |val b8 = Bits(8) <> VAR
         |val u8 = UInt(8) <> VAR init d"8'255"
         |val s8 = SInt(8) <> VAR init ?
         |val u8b = UInt(8) <> VAR init i.resize(8)
         |val u6 = UInt(6) <> IN
         |val s6 = SInt(6) <> IN
         |u8 := i.resize(8)
         |val b6: Bits[6] <> CONST = h"6'00"
         |val s32 = Int <> VAR init 0
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
         |""".stripMargin
    } {
      val c: UInt[8] <> CONST = 1
      val i: Int <> CONST = 100
      val ni: Int <> CONST = -100
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
      val u6 = UInt(6) <> IN
      val s6 = SInt(6) <> IN
      u8 := i
      assertDSLErrorLog(
        "Cannot apply a signed value to an unsigned variable."
      )("") {
        u8 := ni
      }
      assertDSLErrorLog(
        "The applied RHS value width (8) is larger than the LHS variable width (6)."
      )("") {
        s6 := ni
      }
      val b6: Bits[6] <> CONST = all(0)
      val s32: Int <> VAL = Int <> VAR init 0
      assertCompileError(
        "Cannot apply a signed value to an unsigned variable."
      )("u8 := s32")
      assertCompileError(
        "The applied RHS value width (32) is larger than the LHS variable width (6)."
      )("s6 := s32")
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
        "Cannot apply a signed value to an unsigned variable."
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
        "Cannot apply a signed value to an unsigned variable."
      )(
        """u8 := s8"""
      )
      assertCompileError(
        "Cannot apply a signed value to an unsigned variable."
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
    }
    assertDSLErrorLog(
      "Cannot apply a signed value to an unsigned variable."
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
    assertCodeString {
      """|val t1 = u8 == u8
         |val t2 = u8 != d"8'0"
         |val t3 = d"8'0" < u8
         |val t4 = u8 > d"8'12"
         |val t5 = u8 != d"8'255"
         |val t6 = u8 <= b8.uint
         |val t7 = u8.resize(4) >= b8.resize(4).uint
         |""".stripMargin
    } {
      val t1 = u8 == u8
      val t2 = u8 != 0
      val t3 = 0 < u8
      val t4 = u8 > d"8'12"
      val t5 = u8 != h"FF"
      val t6 = u8 <= b8
      val t7 = u8.resize(4) >= b8.resize(4)
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
      """Cannot compare a DFHDL value (width = 8) with a Scala `Int` argument that is wider (width = 10).
        |An explicit conversion must be applied.
        |""".stripMargin
    )(
      """u8 > 1000"""
    ) {
      val value = 1000
      u8 > value
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
    assertEquals(-200 + sd"8'1", sd"9'-199")
    assertCompileError(
      """|Cannot apply this operation between a signed value (LHS) and an unsigned value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """sd"8'22" + d"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """d"8'22" + sd"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """h"8'22" + sd"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """d"8'22" + (-22)"""
    )
    assertCompileError(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """d"8'22" + d"9'22""""
    )
    assertEquals(d"8'22" + 200, d"8'222")
    assertEquals(sd"8'-1" + 1, sd"8'0")
    assertDSLErrorLog(
      "The applied RHS value width (9) is larger than the LHS variable width (8)."
    )(
      """sd"8'22" + 200"""
    ) {
      val value = 200
      sd"8'22" + value
    }
    assertDSLErrorLog(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """22 + sd"8'22""""
    ) {
      val value = 22
      value + sd"8'22"
    }
    assertEquals(d"8'22" - d"8'22", d"8'0")
    assertEquals(sd"8'22" - sd"8'22", sd"8'0")
    assertEquals(sd"8'22" - 22, sd"8'0")
    assertEquals(d"8'22" - b"1001", d"8'13")
    assertEquals(d"8'22" - d"8'23", d"8'255")
    assertEquals(sd"8'22" - sd"8'23", sd"8'-1")
    assertCompileError(
      """|Cannot apply this operation between a signed value (LHS) and an unsigned value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """sd"8'22" - d"8'22""""
    )
    assertCompileError(
      """|Cannot apply this operation between a signed value (LHS) and an unsigned value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
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
    assertDSLErrorLog(
      """|Cannot apply this operation between an unsigned value (LHS) and a signed value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """22 - sd"8'22""""
    ) {
      val value = 22
      value - sd"8'22"
    }

    assertEquals(d"8'22" * d"8'2", d"8'44")
    assertEquals(d"8'22" / d"8'2", d"8'11")
    assertEquals(d"8'22" % d"8'2", d"8'0")
    assertEquals(100 * d"7'2", d"7'72")
    assertEquals(100 / d"7'2", d"7'50")
    assertEquals(17 % d"3'2", d"5'1")
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
    assertCompileError(
      """|Cannot apply this operation between a signed value (LHS) and an unsigned value (RHS).
         |An explicit conversion must be applied.
         |""".stripMargin
    )(
      """s8 + d"8'22""""
    )
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
      """b8 * s8"""
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
end DFDecimalSpec
