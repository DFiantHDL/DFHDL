import DFiant.*
import munit.*
import internals.Inlined
import compiler.printing.{DefaultPrinter, Printer}

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
  given Printer = DefaultPrinter
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
      """h"2'1"""",
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
//    v8 := (h"1", 1, 0, v8(5), true)
  }
end DFBitsSpec
