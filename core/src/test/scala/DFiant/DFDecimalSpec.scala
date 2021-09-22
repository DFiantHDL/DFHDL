package DFiant

import DFiant.*
import munit.*
import compiler.printing.{DefaultPrinter, Printer}
import internals.Inlined

class DFDecimalSpec extends DFSpec:
  val u7 = DFUInt(7)
  val s5 = DFSInt(5)
  test("Inlined width") {
    u7.width.verifyTypeOf[Inlined[7]]
    assert(u7.width.value == 7)
    s5.width.verifyTypeOf[Inlined[5]]
    assert(s5.width.value == 5)
  }
  given Printer = DefaultPrinter
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
  test("Token Conversion") {
    assertEquals(d"255".bits, h"FF")
    assertEquals(h"FF".as(DFUInt(8)), d"255")
    assertEquals(d"8'-1".bits, h"FF")
    assertEquals(h"FF".as(DFSInt(8)), d"8'-1")
  }
  test("DFVal Conversion") {
//    val t1: DFUInt[8] <> VAL = 0
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
