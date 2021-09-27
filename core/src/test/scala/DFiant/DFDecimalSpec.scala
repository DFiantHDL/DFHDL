package DFiant

import DFiant.*
import munit.*
import internals.Inlined

class DFDecimalSpec extends DFSpec:
  test("Type construction safety") {
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
  }

  val u7 = DFUInt(7)
  val s5 = DFSInt(5)
  test("Inlined width") {
    u7.width.verifyInlined(7)
    s5.width.verifyInlined(5)
  }
  test("codeString") {
    assertEquals(u7.codeString, "DFUInt(7)")
    assertEquals(s5.codeString, "DFSInt(5)")
  }
  test("Token Construction") {
    val t1 = (DFUInt(8) token 100).verifyTokenOf[DFUInt[8]]
    val t1b = (DFSInt(8) token -1).verifyTokenOf[DFSInt[8]]
    val t2 = d"255".verifyTokenOf[DFUInt[8]]
    val t3 = d"256".verifyTokenOf[DFUInt[9]]
    val t4 = d"0".verifyTokenOf[DFUInt[1]]
    val t5 = d"10'0".verifyTokenOf[DFUInt[10]]
  }
  test("Token Resize") {
    assertEquals(d"255".resize(4), d"15")
    assertEquals(d"15".resize(8), d"8'15")
    assertEquals(d"-15".resize(8), d"8'-15")
    assertEquals(d"8'-1".resize(4), d"4'-1")
    assertEquals(d"8'-1".resize(8), d"8'-1")
    assertEquals(b"1001".uint.resize(3), d"3'1")
    assertEquals(b"1001".sint.resize(3), d"3'-3")
  }
  test("Token Conversion") {
    assertEquals(d"255".bits, h"FF")
    assertEquals(h"FF".uint, d"255")
    assertEquals(d"8'-1".bits, h"FF")
    assertEquals(h"FF".sint, d"8'-1")
    assertEquals(d"8".signed, DFSInt(5).token(8))
  }
  test("DFVal Conversion") {
    val t1: DFUInt[8] <> VAL = 100
  }
  test("Assignment") {
    val u8 = DFUInt(8) <> VAR init 255
    val s8 = DFSInt(8) <> VAR
    u8 := 0
    u8 := 255
    u8 := d"0"
//    v8 := ?
//    v8 := x
//    v8 := x.bits
////    v8 := (h"1", 1, 0, v8(5), true)
  }
end DFDecimalSpec
