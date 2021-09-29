import DFiant.*
import munit.*
import internals.Inlined

class DFDecimalSpec extends DFSpec:
  test("Type Construction") {
    val zero = 0
    assertDSLError(
      "Unsigned value width must be positive, but found: 0"
    )(
      """DFUInt(0)"""
    ) {
      DFUInt(zero)
    }
    val one = 1
    assertDSLError(
      "Signed value width must be larger than 1, but found: 1"
    )(
      """DFSInt(1)"""
    ) {
      DFSInt(one)
    }
    assertCodeString {
      """val x = DFUInt(8) <> VAR init d"8'0"
        |val y = DFSInt(8) <> VAR init sd"8'-1"
        |val z = DFSInt(8) <> VAR init sd"8'0"
        |""".stripMargin
    } {
      val x = DFUInt(8) <> VAR init 0
      val y = DFSInt(8) <> VAR init -1
      val z = DFSInt(8) <> VAR init 0
    }
  }

  val u7 = DFUInt(7)
  val s5 = DFSInt(5)
  test("Inlined width") {
    u7.width.verifyInlined(7)
    s5.width.verifyInlined(5)
  }
  test("Token Construction") {
    val t1 = (DFUInt(8) token 100).verifyTokenOf[DFUInt[8]]
    val t1b = (DFSInt(8) token -1).verifyTokenOf[DFSInt[8]]
    val t2 = d"255".verifyTokenOf[DFUInt[8]]
    val t3 = d"256".verifyTokenOf[DFUInt[9]]
    val t4 = d"0".verifyTokenOf[DFUInt[1]]
    val t5 = d"10'0".verifyTokenOf[DFUInt[10]]
    val t6 = d"-1".verifyTokenOf[DFSInt[2]]
    val t7 = sd"-1".verifyTokenOf[DFSInt[2]]
    val t8 = sd"0".verifyTokenOf[DFSInt[2]]
    val t9 = DFUInt(8).token(1)
    val t10 = DFUInt(8).token(d"1")
    val t11 = DFUInt(8).token(?)
    val t12 = DFSInt(8).token(1)
    val t13 = DFSInt(8).token(-1)
    val t14 = DFSInt(8).token(?)
    val t15 = DFSInt(8).token(127)
    val t16 = DFSInt(8).token(d"127")
    val t17 = DFSInt(8).token(sd"127")
    assert(t15 == t16)
    assert(t16 == t17)

    assertCompileError("""d"1x"""", "Invalid decimal pattern found: 1x")
    assertCompileError(
      """d"4'255"""",
      "Explicit given width (4) is smaller than the actual width (8)"
    )
    val negOne = -1
    assertDSLError(
      "Unsigned value must be natural, but found: -1"
    )(
      """DFUInt(8).token(-1)"""
    ) {
      DFUInt(8).token(negOne)
    }
    assertCompileError(
      """DFUInt(8).token(sd"1")""",
      "Cannot apply a signed value to an unsigned variable."
    )
    assertDSLError(
      "The token value width (9) is larger than the dataflow value width (8)."
    )(
      """DFSInt(8).token(128)"""
    ) {
      val value = 128
      DFSInt(8).token(value)
    }
    assertDSLError(
      "The token value width (9) is larger than the dataflow value width (8)."
    )(
      """DFSInt(8).token(d"128")"""
    ) {
      val value = 8
      DFSInt(value).token(d"128")
    }
  }
  test("Token Resize") {
    assertEquals(d"255".resize(4), d"15")
    assertEquals(d"15".resize(8), d"8'15")
    assertEquals(d"-15".resize(8), d"8'-15")
    assertEquals(d"8'-1".resize(4), d"4'-1")
    assertEquals(d"8'-1".resize(8), d"8'-1")
    assertEquals(b"1001".uint.resize(3), d"3'1")
    assertEquals(b"1001".sint.resize(3), d"3'-3")
    val zero = 0
    assertDSLError(
      "Unsigned value width must be positive, but found: 0"
    )(
      """d"15".resize(0)"""
    ) {
      d"15".resize(zero)
    }
    val one = 1
    assertDSLError(
      "Signed value width must be larger than 1, but found: 1"
    )(
      """b"1001".sint.resize(1)"""
    ) {
      b"1001".sint.resize(one)
    }
  }
  test("Token Conversion") {
    assertEquals(d"255".bits, h"FF")
    assertEquals(h"FF".uint, d"255")
    assertEquals(d"8'-1".bits, h"FF")
    assertEquals(h"FF".sint, d"8'-1")
    assertEquals(DFUInt(8).token(?).bits, h"??")
    assertEquals(DFSInt(3).token(?).bits, b"???")
    assertEquals(b"111?".uint, DFUInt(4).token(?))
    assertEquals(b"111?".sint, DFSInt(4).token(?))
    assertEquals(d"8".signed, DFSInt(5).token(8))
  }
//  test("DFVal Conversion") {
//    assertCodeString("hi there") {
//      val t0 = DFUInt(6) const 1
//      val t1: DFUInt[8] <> VAL = t0
//    }
//  }
//  test("Assignment") {
//    val u8 = DFUInt(8) <> VAR init 255
//    val s8 = DFSInt(8) <> VAR
//    u8 := 0
//    u8 := 255
//    u8 := d"0"
////    v8 := ?
////    v8 := x
////    v8 := x.bits
//////    v8 := (h"1", 1, 0, v8(5), true)
//  }
end DFDecimalSpec
