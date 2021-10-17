import DFiant.*
import munit.*
import internals.Inlined

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    DFBit.width.verifyInlined(1)
    DFBool.width.verifyInlined(1)
  }

  test("Token Construction") {
    val bool = true
    val t1: DFBit <> TOKEN = 0
    val t2: DFBit <> TOKEN = DFBit token 1
    val t3: DFBool <> TOKEN = DFBool token true
    val t4: DFBool <> TOKEN = false
    val t5: DFBit <> TOKEN = DFBit token ?
    val t6: DFBool <> TOKEN = DFBool token t5
    val t7: DFBool <> TOKEN = DFBool token bool
    val t8: DFBit <> TOKEN = t7
    val t9: DFBool <> TOKEN = t8
  }
  test("Token Conversion") {
//    assertEquals(d"8".signed, DFSInt(5).token(8))
  }
  test("DFVal Conversion") {}
  test("Assignment") {}
  test("Comparison") {}
  test("Logical Ops") {
    val bit0 = DFBit token 0
    val bit1 = DFBit token 1
    val boolF = DFBool token false
    val boolT = DFBool token true
    assertEquals(!bit0, bit1)
    assertEquals(!bit1, bit0)
    assertEquals(!boolF, boolT)
    assertEquals(!boolT, boolF)
    assertEquals(bit0.bool, boolF)
    assertEquals(boolT.bit, bit1)
    assertEquals(bit0 || 1, bit1)
    assertEquals(1 && bit0, bit0)
    assertEquals(bit0 ^ false, bit0)
    assertEquals(false || bit0, bit0)
    assertEquals(boolF && 1, boolF)
    assertEquals(1 || boolF, boolT)
    assertEquals(boolF && bit0, boolF)
    assertEquals(bit1 ^ boolT, bit0)
  }
end DFBoolOrBitSpec
