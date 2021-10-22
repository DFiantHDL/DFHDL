import DFiant.*
import munit.*
import internals.Inlined

class DFBitsSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLError(
      "Width must be positive, but found: 0"
    )(
      """DFBits(0)"""
    ) {
      DFBits(zero)
    }
  }
  test("Inlined width") {
    val b8 = DFBits(8)
    b8.width.verifyInlined(8)
  }
  test("Token Construction") {
    val t1 = (DFBits(8) token b0s).verifyTokenOf[DFBits[8]]
    val t1b = (DFBits(8) token b1s).verifyTokenOf[DFBits[8]]
    val t2 = h"12".verifyTokenOf[DFBits[8]]
    val t3 = h"10'12".verifyTokenOf[DFBits[10]]
    val t4 = b"11".verifyTokenOf[DFBits[2]]
    val t5 = h"1{00}1".verifyTokenOf[DFBits[10]]
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

    val t6 = (DFBits(3) token ?).verifyTokenOf[DFBits[3]]
    val t7 = (DFBits(8) token t2).verifyTokenOf[DFBits[8]]
    assertCompileError(
      "The token width (8) is different than the DFType width (3)."
    )("""DFBits(3) token t7""")
    assert(t7.asIR equals t2.asIR)
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
    assertEquals(b"10".apply(0), DFBit.token(0))
    assertEquals(b"10".apply(1), DFBit.token(1))
    assertEquals(b"1?".apply(0), DFBit.token(?))
    assertEquals(b"10".lsbit, DFBit.token(0))
    assertEquals(b"10".msbit, DFBit.token(1))
    assert(!(b"10".msbit.asIR equals DFBit.token(0).asIR))
    val four = 4
    assertDSLError(
      "Index 4 is out of range of width/length 2"
    )(
      """b"10".apply(4)"""
    ) {
      b"10".apply(four)
    }
    assertEquals(b"1010".apply(3, 2), b"10")
    assertEquals(b"1010".apply(1, 0), b"10")
    assertEquals(b"1010".apply(2, 1), b"01")
    assertEquals(b"1010".apply(2, 2), b"0")

    assertDSLError(
      "Index 4 is out of range of width/length 4"
    )(
      """b"1010".apply(4, 2)"""
    ) {
      b"1010".apply(four, 2)
    }
    val negOne = -1
    assertDSLError(
      "Index -1 is out of range of width/length 4"
    )(
      """b"1010".apply(3, -1)"""
    ) {
      b"1010".apply(3, negOne)
    }
    val three = 3
    assertDSLError(
      "Low index 3 is bigger than High bit index 2"
    )(
      """b"1010".apply(2, 3)"""
    ) {
      b"1010".apply(2, three)
    }
  }
  test("DFVal Conversion") {
    val t1: DFBits[8] <> VAL = b0s
    val t2: DFBits[8] <> VAL = b1s
    val t3: DFBits[8] <> VAL = d"255"
    val t4: DFBits[5] <> VAL = ?
    val t5: DFBits[4] <> VAL = h"A"
    val t6: DFBits[3] <> VAL = b"101"
    val t7: DFBits[Int] <> VAL = b"11"
  }
  test("Assignment") {
    val b8 = DFBits(8) <> VAR
    val u8 = DFUInt(8) <> VAR
    assertCodeString {
      """|b8 := h"8'11"
         |b8 := h"8'00"
         |b8 := h"8'ff"
         |b8 := h"8'??"
         |b8 := u8.bits
         |b8 := u8.bits
         |b8 := (h"4'1", b"1", b"0", b"11")
         |b8 := (u8.bits(3, 0), u8.bits(7, 4))
         |""".stripMargin
    } {
      b8 := h"11"
      b8 := b0s
      b8 := b1s
      b8 := ?
      b8 := u8
      b8 := u8.bits
      b8 := (h"1", 1, 0, b"11")
      b8 := (u8.bits(3, 0), u8.bits(7, 4))
    }
    val twelve = 12
    val v12 = DFBits(twelve) <> VAR
    assertDSLError(
      """|The argument width (12) is different than the reciever width (8).
         |Consider applying `.resize` to resolve this issue.""".stripMargin
    )(
      """b8 := h"123""""
    ) {
      b8 := v12
    }
  }
  test("Comparison") {
    val b8 = DFBits(8) <> VAR
    val u8 = DFUInt(8) <> VAR
    assertCodeString(
      """|val t1 = b8 == h"8'00"
         |val t2 = b8 != h"8'ff"
         |val t3 = b8 == u8.bits
         |val t4 = b8 == h"8'0c"
         |val t5 = b8 != h"8'22"
         |val t6 = b8 == h"8'e7"
         |val t7 = b8 == (u8.bits(3, 0), u8.bits(7, 4))
         |""".stripMargin
    ) {
      val t1 = b8 == b0s
      val t2 = b8 != b1s
      val t3 = b8 == u8
      val t4 = b8 == d"8'12"
      val t5 = b8 != h"22"
      val t6 = b8 == b"11100111"
      val t7 = b8 == (u8.bits(3, 0), u8.bits(7, 4))
    }
    assertCompileError {
      """An integer value cannot be a candidate for a DFBits type.
        |Try explicitly using a decimal token via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == 25")
    val num = 25
    assertCompileError {
      """An integer value cannot be a candidate for a DFBits type.
        |Try explicitly using a decimal token via the `d"<width>'<number>"` string interpolation.
        |""".stripMargin
    }("b8 == num")
  }
end DFBitsSpec
