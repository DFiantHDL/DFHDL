package CoreSpec
import dfhdl.*
import munit.*

class DFBitsSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLErrorLog(
      "Width must be positive, but found: 0"
    )(
      """Bits(0)"""
    ) {
      Bits(zero)
    }
  }
  test("Inlined width") {
    val b8 = Bits(8)
    b8.verifyWidth(8)
  }
  test("DFVal Conversion") {
    val w = 2
    val param: Int <> CONST = 5
    assertCodeString {
      """|val t1: Bits[8] <> CONST = h"00"
         |val t2: Bits[8] <> CONST = h"ff"
         |val t3: Bits[8] <> CONST = h"ff"
         |val t4: Bits[5] <> CONST = h"5'??"
         |val t5: Bits[4] <> CONST = h"a"
         |val t6: Bits[3] <> CONST = b"101"
         |val t7: Bits[2] <> CONST = b"11"
         |val t8: Bits[8] <> CONST = (b"100", b"1", h"9").toBits
         |val t9: Bits[8] <> CONST = (b"100", b"1", h"9").toBits
         |val t10: Bits[8] <> CONST = (b"100", b"1", h"9").toBits
         |val t11 = twice(t1)
         |val t12: Bits[16] <> CONST = t1.repeat(2)
         |val t13: Bits[8] <> CONST = (h"9", h"2").toBits
         |val t14: Bits[16] <> CONST = t13.repeat(2)
         |val u8v = Bits(8) <> VAR init (h"aa", h"bb", (h"c", h"d").toBits)
         |val t15 = Bits(param) <> VAR init b"0".repeat(param)
         |val t16 = Bits(param) <> VAR init b"1".repeat(param)
         |val t17 = Bits(3) <> VAR
         |val t18 = Bits(clog2(param)) <> VAR
         |val t19 = Bits(4) <> VAR
         |val t20 = Bits(clog2(param + 1)) <> VAR
         |val t21: Bits[16] <> CONST = (t14 << t5.uint.toInt) | (t14 >> t5.uint.toInt)
         |""".stripMargin
    } {
      @inline def foo(arg: Bits[4] <> CONST): Unit <> DFRET =
        arg.assertPosition(-1, 1, 11, 17)
      foo(all(0))
      val t1: Bits[8] <> CONST = all(false); t1.assertPosition(0, 1, 34, 44)
      val t2: Bits[8] <> CONST = all(1)
      val t3: Bits[8] <> CONST = d"255"
      val t4: Bits[5] <> CONST = ?
      val t5: Bits[4] <> CONST = h"A"; t5.assertPosition(0, 1, 34, 38)
      val t6: Bits[3] <> CONST = b"101"
      val t7: Bits[w.type] <> CONST = b"11"
      val t8: Bits[8] <> CONST = (b"100", 1, h"9"); t8.assertPosition(0, 1, 34, 51)
      val t9: Bits[Int] <> CONST = (b"100", 1, h"9")
      val t10 = (
        b"100",
        1,
        h"9"
      ).toBits; t10.assertPosition(0, 5, 17, 15)
      def twice(value: Bits[Int] <> VAL): Bits[Int] <> DFRET = (value, value)
      val t11 = twice(t1); t11.assertPosition(0, 1, 17, 26)
      assertLatestDesignDclPosition(2, 1, 7, 78)
      assert(t11.widthInt == 16)
      @inline def twiceInline(value: Bits[Int] <> VAL): Bits[Int] <> DFRET = (value, value)
      val t12 = twiceInline(t1); t12.assertPosition(0, 1, 17, 32)
      assert(t12.widthInt == 16)
      val t13: Bits[8] <> CONST = (b"1001", h"2")
      val t14: Bits[16] <> CONST = (t13, t13)
      val u8v = Bits(8) <> VAR init (h"aa", h"bb", (h"c", h"d"))
      val lastConsts = dfc.mutableDB.DesignContext.getLastMembers(6)
      lastConsts(0).assertPosition(2, 1, 38, 43)
      lastConsts(1).assertPosition(3, 1, 45, 50)
      // lastConsts(2).assertPosition(4, 1, 53, 57) TODO: needs to fix positioning (currently 52-64)
      // lastConsts(3).assertPosition(5, 1, 59, 63) TODO: needs to fix positioning (currently 52-64)
      lastConsts(4).assertPosition(6, 1, 52, 64)
      assertCompileError {
        "Applied argument must be a constant."
      }("val t15: Bits[16] <> CONST = (u8v, t13)")
      val t15 = Bits(param) <> VAR init all(0)
      val t16 = Bits[param.type] <> VAR init all(1)
      val t17 = Bits.until(8) <> VAR
      val t18 = Bits.until(param) <> VAR
      val t19 = Bits.to(8) <> VAR
      val t20 = Bits.to(param) <> VAR
      val t21: Bits[16] <> CONST = t14 << t5 | t14 >> t5
    }
  }
  test("Assignment") {
    val b8 = Bits(8) <> VAR
    val b4M, b4L = Bits(4) <> VAR
    val b3M = Bits(3) <> VAR
    val u5L = UInt(5) <> VAR
    val u8 = UInt(8) <> VAR
    assertCodeString {
      """|val byte = Bits(8) <> VAR init h"00"
         |b8 := h"11"
         |b8 := h"00"
         |b8 := h"ff"
         |b8 := h"??"
         |b8 := u8.bits
         |b8 := u8.bits
         |b8 := b3M.resize(8)
         |b3M := b8.resize(3)
         |b8 := (h"1", b"1", b"0", b"11").toBits
         |b4M := h"1"
         |b4L := (b"1", b"0", b"11").toBits
         |b3M := b"000"
         |u5L := (b"1", b"1", b"0", b"11").toBits.uint
         |b8 := (u8.bits(3, 0), u8.bits(7, 4)).toBits
         |b4M := u8.bits(3, 0)
         |b4L := u8.bits(7, 4)
         |b4M := u8.bits(7, 4)
         |b3M := u8.bits(3, 1)
         |u5L := (u8.bits(0, 0), b8(7, 4)).toBits.uint
         |b4L := b8(3, 0)
         |""".stripMargin
    } {
      val byte: Byte <> VAL = Byte <> VAR init all(0)
      b8 := h"11"
      b8 := all(0)
      b8 := all(1)
      b8 := ?
      b8 := u8
      b8 := u8.bits
      b8 := b3M.extend
      b3M := b8.truncate
      b8 := (h"1", 1, 0, b"11").toBits
      (b4M, b4L) := (h"1", 1, 0, b"11")
      (b3M, u5L) := (h"1", 1, 0, b"11")
      b8 := (u8.bits(3, 0), u8.bits(7, 4))
      (b4M, b4L) := (u8.bits(3, 0), u8.bits(7, 4))
      (b4M, b3M, u5L, b4L) := (u8, b8)
    }
    val twelve = 12
    val v12 = Bits(twelve) <> VAR
    assertDSLErrorLog(
      """|The argument width (12) is different than the receiver width (8).
         |Consider applying `.resize` to resolve this issue.""".stripMargin
    )(
      """b8 := h"123""""
    ) {
      b8 := v12
    }
    assertDSLErrorLog(
      """|The argument width (12) is different than the receiver width (8).
         |Consider applying `.resize` to resolve this issue.""".stripMargin
    )(
      """val conv8: Bits[8] <> VAL = h"123""""
    ) {
      val w = 8
      val conv8: Bits[w.type] <> VAL = h"123"
    }
    assertCompileError {
      """An integer value cannot be a candidate for a Bits type.
        |Try explicitly using a decimal constant via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 := b8 | 2")
  }
  test("DFVal Selection") {
    val b8 = Bits(8) <> VAR
    val b3 = Bits(3) <> VAR
    assertCodeString {
      """|val ms8 = b8(7, 0)
         |val ms7 = b8(7, 1)
         |val ms1 = b8(7, 7)
         |val ls8 = b8(7, 0)
         |val ls7 = b8(6, 0)
         |val ls1 = b8(0, 0)
         |val bit2 = b3(2)
         |val bit0 = b3(0)
         |val b8_msbit = b8(7)
         |val b8_lsbit = b8(0)
         |""".stripMargin
    } {
      val ms8 = b8.msbits(8)
      val ms7 = b8.msbits(7)
      val ms1 = b8.msbits(1)
      val ls8 = b8.lsbits(8)
      val ls7 = b8.lsbits(7)
      val ls1 = b8.lsbits(1)
      val bit2 = b3(2)
      val bit0 = b3(0)
      val b8_msbit = b8.msbit
      val b8_lsbit = b8.lsbit
    }
    val nine = 9
    assertDSLErrorLog(
      """The new width (9) is larger than the original width (8)."""
    )(
      """b8.msbits(9)"""
    ) {
      b8.msbits(nine)
    }
    assertDSLErrorLog(
      """The new width (9) is larger than the original width (8)."""
    )(
      """b8.lsbits(9)"""
    ) {
      b8.lsbits(nine)
    }
    assertDSLErrorLog(
      "The argument must be smaller than the upper-bound 3 but found: 3"
    )(
      """b3(3)"""
    ) {
      val three = 3
      b3(three)
    }
  }
  test("Comparison") {
    val b8 = Bits(8) <> VAR
    val u8 = UInt(8) <> VAR
    assertCodeString(
      """|val t1 = b8 == h"00"
         |val t2 = b8 != h"ff"
         |val t3 = b8 == u8.bits
         |val t4 = b8 == h"0c"
         |val t5 = b8 != h"22"
         |val t6 = b8 == h"e7"
         |val t7 = b8 == (u8.bits(3, 0), u8.bits(7, 4)).toBits
         |""".stripMargin
    ) {
      val t1 = b8 == all(0)
      val t2 = b8 != all(1)
      val t3 = b8 == u8
      val t4 = b8 == d"8'12"
      val t5 = b8 != h"22"
      val t6 = b8 == b"11100111"
      val t7 = b8 == (u8.bits(3, 0), u8.bits(7, 4))
    }
    assertCompileError {
      """An integer value cannot be a candidate for a Bits type.
        |Try explicitly using a decimal constant via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == 25")
    val num = 25
    assertCompileError {
      """An integer value cannot be a candidate for a Bits type.
        |Try explicitly using a decimal constant via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == num")
  }
  test("Bit reduction operations") {
    val x = b"1001"
    val y = b"0001"
    val z = b"1000"
    val w = b"1111"
    assert(x.|.toScalaBoolean == true)
    assert(x.&.toScalaBoolean == false)
    assert(x.^.toScalaBoolean == false)
    assert(w.&.toScalaBoolean == true)
    assert((x ^ y ^ z).|.toScalaBoolean == false)
  }
  test("Operations") {
    val b8 = Bits(8) <> VAR
    val shift: Int <> CONST = 2
    val sel: Int <> CONST = 3
    assertCodeString(
      """|val t1 = b8 << shift
         |val t2 = b8 >> shift
         |val t3 = b8(sel)""".stripMargin
    ) {
      val t1 = b8 << shift
      val t2 = b8 >> shift
      val t3 = b8(sel)
    }
    assertDSLErrorLog(
      "Argument must be unsigned"
    )(
      """b8 << -2"""
    ) {
      val bad_shift: Int <> CONST = -1
      b8 << bad_shift
    }
    val s3 = SInt(3) <> VAR
    assertCompileError(
      "Argument must be unsigned"
    )(
      """b8(s3)"""
    )
    assertDSLErrorLog(
      "The argument must be smaller than the upper-bound 8 but found: 10"
    )(
      """b8 >> 10"""
    ) {
      val bad_shift: Int <> CONST = 10
      b8 >> bad_shift
    }
    val u5 = UInt(5) <> VAR
    assertCompileError(
      "Expected argument width 3 but found: 5"
    )(
      """b8(u5)"""
    )
  }
end DFBitsSpec
