//import DFiant.*
//import munit.*
//import internals.Inlined
//
//class DFTokenSpec extends FunSuite:
//  test("DFBit Token Construction") {
//    val bool = true
//    val t1: DFBit.Token = DFBit token 0
//    val t2: DFBit.Token = DFBit token 1
//    val t3: DFBool.Token = DFBool token true
//    val t4: DFBool.Token = DFBool token false
//    val t5: DFBit.Token = DFBit token ?
//    val t6: DFBool.Token = DFBool token t5
//    val t7: DFBool.Token = DFBool token bool
//    assert(t3 == t7)
//  }
//
//  test("DFBits Token Construction") {
//    val t1: DFBits.Token[8] = DFBits(8) token b0s
//    val t1b: DFBits.Token[8] = DFBits(8) token b1s
//    val t2: DFBits.Token[8] = h"12"
//    val t3: DFBits.Token[10] = h"10'12"
//    val t4: DFBits.Token[2] = b"11"
//    val t5: DFBits.Token[10] = h"1{00}1"
//    val t6: DFBits.Token[3] = DFBits(3) token ?
//    val t7: DFBits.Token[8] = DFBits(8) token t2
//    assert(t7 == t2)
//  }
//
//  test("DFTuple Token Construction") {
//    val t1 = (DFBits(8), DFBits(8)) token (h"11", b"10010110")
//    val tplDef = (DFBits(8), DFBits(8))
//    val tplValue = (h"11", b"10010110")
//    val t2 = tplDef token (h"11", b"10010110")
//    val t3 = tplDef token tplValue
//    val z = (DFBit, DFBits(8), (DFBit, DFBool)) token (1, h"11", (true, 0))
//    assert(t1 == t2)
//    assert(t2 == t3)
//  }
