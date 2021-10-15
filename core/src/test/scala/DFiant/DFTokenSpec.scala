import DFiant.*
import munit.*
import internals.Inlined

class DFTokenSpec extends FunSuite:
  test("DFBit/DFBool Token Construction and Conversion") {
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
//    assert(t3 == t7)
  }

  test("DFTuple Token Construction") {
    val t1 = (DFBits(8), DFBits(8)) token (h"11", b"10010110")
    val tplDef = (DFBits(8), DFBits(8))
    val tplValue = (h"11", b"10010110")
    val t2 = tplDef token (h"11", b"10010110")
    val t3 = tplDef token tplValue
    val z = (DFBit, DFBits(8), (DFBit, DFBool)) token (1, h"11", (true, 0))
//    assert(t1 == t2)
//    assert(t2 == t3)
  }
end DFTokenSpec
