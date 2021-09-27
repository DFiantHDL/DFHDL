import DFiant.*
import munit.*
import internals.Inlined

class DFBitsSpec extends DFSpec:
  test("Type construction safety") {
    val zero = 0
    assertDSLError(
      "Width must be positive, but found: 0"
    )(
      """DFBits(0)"""
    ) {
      DFBits(zero)
    }
  }
  val b8 = DFBits(8)
  test("Inlined width") {
    b8.width.verifyInlined(8)
  }
  test("codeString") {
    assertEquals(b8.codeString, "DFBits(8)")
  }
  test("DFBits Token Construction") {
    val t1 = (DFBits(8) token b0s).verifyTokenOf[DFBits[8]]
    val t1b = (DFBits(8) token b1s).verifyTokenOf[DFBits[8]]
    val t2 = h"12".verifyTokenOf[DFBits[8]]
    val t3 = h"10'12".verifyTokenOf[DFBits[10]]
    val t4 = b"11".verifyTokenOf[DFBits[2]]
    val t5 = h"1{00}1".verifyTokenOf[DFBits[10]]
    assertCompileError("""h"1{001"""", "Missing closing braces of binary mode")
    assertCompileError("""h"1x"""", "Found invalid hex character: x")
    assertCompileError(
      """h"2'F"""",
      "Explicit given width (2) is smaller than the actual width (4)"
    )
    assertCompileError(
      """h"12{12}"""",
      "Found invalid binary character in binary mode: 2"
    )
    assertCompileError("""b"1x"""", "Found invalid binary character: x")
    assertCompileError(
      """b"2'111"""",
      "Explicit given width (2) is smaller than the actual width (3)"
    )

    val t6 = (DFBits(3) token ?).verifyTokenOf[DFBits[3]]
    val t7 = (DFBits(8) token t2).verifyTokenOf[DFBits[8]]
    assertCompileError(
      """DFBits(3) token t7""",
      "The token width (8) is different than the DFType width (3)."
    )
    assert(t7 == t2)
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
    assert(b"10".msbit != DFBit.token(0))
    val four = 4
    assertDSLError(
      "Index 4 is out of range of width/length 2"
    )(
      """b"10".apply(4)"""
    ) {
      b"10".apply(four)
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
    val v8 = DFBits(8) <> VAR
    val x = DFUInt(8) <> VAR
    v8 := h"11"
    v8 := b0s
    v8 := b1s
    v8 := ?
    v8 := x
    v8 := x.bits

    val twelve = 12
    val v12 = DFBits(twelve) <> VAR

    assertDSLError(
      """|The argument width (12) is different than the reciever width (8).
         |Consider applying `.resize` to resolve this issue.""".stripMargin
    )(
      """v8 := h"123""""
    ) {
      v8 := v12
    }
    // v8 := (h"1", 1, 0, v8(5), true)
  }
end DFBitsSpec
