package CoreSpec
import dfhdl.*
import munit.*

class DFTokenSpec extends FunSuite:
  test("DFTuple Token Construction") {
    val t1 = (Bits(8), Bits(8)) token (h"11", b"10010110")
    val tplDef = (Bits(8), Bits(8))
    val tplValue = (h"11", b"10010110")
    val t2 = tplDef token (h"11", b"10010110")
    val t3 = tplDef token tplValue
    val z = (Bit, Bits(8), (Bit, Boolean)) token (1, h"11", (true, 0))
//    assert(t1 == t2)
//    assert(t2 == t3)
  }
end DFTokenSpec
