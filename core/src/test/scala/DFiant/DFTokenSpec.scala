import DFiant.*
import munit.*
import internals.Inlined

class DFTokenSpec extends FunSuite:
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
