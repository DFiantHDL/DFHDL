import DFiant.*
import munit.*
import internals.{AllowTopLevel, Inlined}
import compiler.printing.{DefaultPrinter, Printer}

import scala.annotation.internal.sharable
import collection.immutable.ListMap

class DFBitsSpec extends FunSuite, AllowTopLevel:
  val b8 = DFBits(8)
  test("Inlined width") {
    val a: Inlined.Int[8] = b8.width
    assert(b8.width.value == 8)
  }
  given Printer = DefaultPrinter
  test("codeString") {
    assertEquals(b8.codeString, "DFBits(8)")
  }
  test("DFBits Token Construction") {
    val t1: DFBits[8] <> TOKEN = DFBits(8) token b0s
    val t1b: DFBits[8] <> TOKEN = DFBits(8) token b1s
    val t2: DFBits[8] <> TOKEN = h"12"
    val t3: DFBits[10] <> TOKEN = h"10'12"
    val t4: DFBits[2] <> TOKEN = b"11"
    val t5: DFBits[10] <> TOKEN = h"1{00}1"
    val t6: DFBits[3] <> TOKEN = DFBits(3) token ?
    val t7: DFBits[8] <> TOKEN = DFBits(8) token t2
    //    val u: DFUInt[8] <> TOKEN = ???
    //    val t8: DFBits[8] <> TOKEN = u
    assert(t7 == t2)
  }
  test("Assignment") {
    val v8 = DFBits(8) <> VAR
    val x = DFUInt(8) <> VAR
    v8 := h"11"
    v8 := b0s
    v8 := b1s
    v8 := ?
    //TODO: fix
//    v8 := x
    v8 := x.bits
    v8 := (h"1", 1, 0, v8(5), true)
  }
end DFBitsSpec
