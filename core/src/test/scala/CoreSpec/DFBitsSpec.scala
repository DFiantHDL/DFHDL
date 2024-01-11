package CoreSpec
import dfhdl.*
import munit.*

class DFBitsSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLError(
      "Width must be positive, but found: 0"
    )(
      """Bits(0)"""
    ) {
      Bits(zero)
    }
  }
  test("Inlined width") {
    val b8 = Bits(8)
    b8.width.verifyInlined(8)
  }
  test("Token Construction") {
    val t1 = (Bits(8) token all(0)).verifyTokenOf[Bits[8]]
    val t1b = (Bits(8) token all(true)).verifyTokenOf[Bits[8]]
    val t2 = h"12".verifyTokenOf[Bits[8]]
    val t3 = h"10'12".verifyTokenOf[Bits[10]]
    val t4 = b"11".verifyTokenOf[Bits[2]]
    val t5 = h"1{00}1".verifyTokenOf[Bits[10]]
    assertCompileError("Missing closing braces of binary mode")("""h"1{001"""")
    assertCompileError("Found invalid hex character: x")("""h"1x"""")
    assertCompileError(
      "Explicit given width (2) is smaller than the actual width (4)"
    )("""h"2'F"""")
    assertCompileError("Found invalid binary character in binary mode: 2")(
      """h"12{12}""""
    )
    assertCompileError("Found invalid binary character: x")("""b"1x"""")
    assertCompileError(
      "Explicit given width (2) is smaller than the actual width (3)"
    )("""b"2'111"""")

    val t6 = (Bits(3) token ?).verifyTokenOf[Bits[3]]
    val t7 = (Bits(8) token t2).verifyTokenOf[Bits[8]]
    assertCompileError(
      "The token width (8) is different than the DFType width (3)."
    )("""Bits(3) token t7""")
    assert(t7.asIR equals t2.asIR)
    val t8: Bits[8] <> TOKEN = all(0)
    val t9: Bits[8] <> TOKEN = h"22"
    assertDSLError(
      "The token width (4) is different than the DFType width (8)."
    )(
      """val t10: Bits[8] <> TOKEN = h"2""""
    ) {
      val eight = 8
      val t10: Bits[eight.type] <> TOKEN = h"2"
    }
    val t10 = (Bits(8) token (h"A", h"7")).verifyTokenOf[Bits[8]]
    assertEquals(t10, h"A7")
//    val t11: Bits[8] <> TOKEN = (h"A", h"7")
  }
  test("Token Resize") {
    assertEquals(h"F0".resize(6), h"6'30")
    assertEquals(h"F0".resize(1), b"0")
    assertEquals(h"F1".resize(1), b"1")
    assertEquals(b"1".resize(1), b"1")
    assertEquals(b"1".resize(8), h"01")
    assertEquals(b"0".resize(8), h"00")
    val zero = 0
    assertDSLError(
      "Width must be positive, but found: 0"
    )(
      """h"F1".resize(0)"""
    ) {
      h"F1".resize(zero)
    }
  }
  test("Token Bits Selection") {
    val t1 = b"10"
    val t2 = b"1?"
    assertEquals(t1(0), Bit.token(0))
    assertEquals(t1(1), Bit.token(1))
    assertEquals(b"10".lsbit, Bit.token(0))
    assertEquals(b"10".msbit, Bit.token(1))
    assertEquals(t2(0), Bit.token(?))
    assertNotEquals(b"10".msbit.asIR, Bit.token(0).asIR)
    val four = 4
    assertDSLError(
      "Index 4 is out of range of width/length 2"
    )(
      """t1(4)"""
    ) {
      t1(four)
    }
    val t3 = b"1010"
    assertEquals(t3(3, 2), b"10")
    assertEquals(t3(1, 0), b"10")
    assertEquals(t3(2, 1), b"01")
    assertEquals(t3(2, 2), b"0")

    assertDSLError(
      "Index 4 is out of range of width/length 4"
    )(
      """t3(4, 2)"""
    ) {
      t3(four, 2)
    }
    val negOne = -1
    assertDSLError(
      "Index -1 is out of range of width/length 4"
    )(
      """t3(3, -1)"""
    ) {
      t3(3, negOne)
    }
    val three = 3
    assertDSLError(
      "Low index 3 is bigger than High bit index 2"
    )(
      """t3(2, 3)"""
    ) {
      t3(2, three)
    }
  }
  test("DFVal Conversion") {
    val w = 2
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
         |""".stripMargin
    } {
      val t1: Bits[8] <> VAL = all(false); t1.assertPosition(0, 1, 32, 42)
      val t2: Bits[8] <> VAL = all(1)
      val t3: Bits[8] <> VAL = d"255"
      val t4: Bits[5] <> VAL = ?
      val t5: Bits[4] <> VAL = h"A"; t5.assertPosition(0, 1, 32, 36)
      val t6: Bits[3] <> VAL = b"101"
      val t7: Bits[w.type] <> VAL = b"11"
      val t8: Bits[8] <> VAL = (b"100", 1, h"9"); t8.assertPosition(0, 1, 32, 49)
      val t9: Bits[Int] <> VAL = (b"100", 1, h"9")
      val t10 = (
        b"100",
        1,
        h"9"
      ).toBits; t10.assertPosition(0, 5, 17, 15)
      def twice(value: Bits[Int] <> VAL): Bits[Int] <> DFRET = (value, value)
      val t11 = twice(t1); t11.assertPosition(0, 1, 17, 26)
      assertLatestDesignDclPosition(2, 1, 7, 78)
      assert(t11.width == 16)
      @inline def twiceInline(value: Bits[Int] <> VAL): Bits[Int] <> DFRET = (value, value)
      val t12 = twiceInline(t1); t12.assertPosition(0, 1, 17, 32)
      assert(t12.width == 16)
      val t13: Bits[8] <> CONST = (b"1001", h"2")
      val t14: Bits[16] <> CONST = (t13, t13)
      assertCompileError {
        "Applied argument is not a constant."
      }("val t15: Bits[16] <> CONST = (t8, t13)")
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
        |Try explicitly using a decimal token via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == 25")
    val num = 25
    assertCompileError {
      """An integer value cannot be a candidate for a Bits type.
        |Try explicitly using a decimal token via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == num")
  }
end DFBitsSpec
