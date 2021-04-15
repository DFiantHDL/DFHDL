package DFiant
import shapeless.test.illTyped

class DFBitsSpec extends DFSpec {
  val one = 1
  val two = 3
  val three = 3
  val four = 4
  val negOne = -1
  val a = DFBits(4) <> VAR
  test("Construction with positive widths") {
    assert(compileErrors("""val a : DFBits[4] = DFBits(4) <> VAR""").isEmpty)
    assert(compileErrors("""val a : DFBits[4] = DFBits[4] <> VAR""").isEmpty)
    assert(compileErrors("""val a : DFBits[Int] = DFBits(four) <> VAR""").isEmpty)
  }
  test("Construction with negative or zero widths") {
    assert(compileErrors("""val a = DFBits[0] <> VAR""").nonEmpty)
    assert(compileErrors("""val a = DFBits[-1] <> VAR""").nonEmpty)
    //intercept[IllegalArgumentException] {val a = DFBits(negOne) <> VAR}
  }
  test("Bits High-Low Selection within its width boundary") {
    assert(compileErrors("""val b : DFBits[2] = a(1, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[4] = a(3, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[1] = a(2, 2)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a(three, 2)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a(3, three)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a(three, one)""").isEmpty)
    assert(compileErrors("""val b : DFBits[2] = a.bits(1, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[4] = a.bits(3, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[1] = a.bits(2, 2)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a.bits(three, 2)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a.bits(3, three)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a.bits(three, one)""").isEmpty)
  }
  test("Bits High-Low Selection outside the boundary") {
    assert(compileErrors("""val b = a(1, -1)""").nonEmpty)
    assert(compileErrors("""val b = a(5, 0)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a(1, negOne)}
    //intercept[IllegalArgumentException] {val b = a(four, 1)}
    assert(compileErrors("""val b = a.bits(1, -1)""").nonEmpty)
    assert(compileErrors("""val b = a.bits(5, 0)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a.bits(1, negOne)}
    //intercept[IllegalArgumentException] {val b = a.bits(four, 1)}
  }
  test("Bits High-Low Selection low index higher than the high index") {
    assert(compileErrors("""val b = a(1, 2)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a(1, three)}
    assert(compileErrors("""val b = a.bits(1, 2)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a.bits(1, three)}
  }
  test("Bits Width-Low Selection within its width boundary") {
    assert(compileErrors("""val b : DFBits[2] = a.bitsWL(2, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[4] = a.bitsWL(4, 0)""").isEmpty)
    assert(compileErrors("""val b : DFBits[1] = a.bitsWL(1, 2)""").isEmpty)
    assert(compileErrors("""val b : DFBits[4] = a.bitsWL(4, two)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a.bitsWL(one, 3)""").isEmpty)
    assert(compileErrors("""val b : DFBits[Int] = a.bitsWL(three, one)""").isEmpty)
  }
  test("Bits Width-Low Selection outside the boundary or non-positive width") {
    assert(compileErrors("""val b = a.bitsWL(1, -1)""").nonEmpty)
    assert(compileErrors("""val b = a.bitsWL(1, 5)""").nonEmpty)
    assert(compileErrors("""val b = a.bitsWL(0, 1)""").nonEmpty)
    assert(compileErrors("""val b = a.bitsWL(2, 3)""").nonEmpty)
    assert(compileErrors("""val b = a.bitsWL(-1, 1)""").nonEmpty)
//    //intercept[IllegalArgumentException] {val b = a.bits(1, negOne)}
//    //intercept[IllegalArgumentException] {val b = a.bits(four, 1)}
  }
  test("Single Bit Selection within its width boundary") {
    assert(compileErrors("""val b : DFBit = a(0)""").isEmpty)
    assert(compileErrors("""val b : DFBit = a(3)""").isEmpty)
    assert(compileErrors("""val b : DFBit = a(three)""").isEmpty)
    assert(compileErrors("""val b : DFBit = a.bit(0)""").isEmpty)
    assert(compileErrors("""val b : DFBit = a.bit(3)""").isEmpty)
    assert(compileErrors("""val b : DFBit = a.bit(three)""").isEmpty)
  }
  test("Single Bit Selection not allow bit index access outside the boundary") {
    assert(compileErrors("""val b = a(-1)""").nonEmpty)
    assert(compileErrors("""val b = a(5)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a(negOne)}
    //intercept[IllegalArgumentException] {val b = a(four)}
    assert(compileErrors("""val b = a.bit(-1)""").nonEmpty)
    assert(compileErrors("""val b = a.bit(5)""").nonEmpty)
    //intercept[IllegalArgumentException] {val b = a.bit(negOne)}
    //intercept[IllegalArgumentException] {val b = a.bit(four)}
  }
  test("Initialization with BitVectors") {
    assert(compileErrors("""val aInit = a init b"1000"""").isEmpty)
    assert(compileErrors("""val aInit = a init h"F"""").isEmpty)
    assert(compileErrors("""val aInit = a.init(h"1", h"2", h"F")""").isEmpty)
  }
  test("Initialization with different lengths than the constructed variable") {
    val i = b"101"
    assert(compileErrors("""val aInit = a init b"1"""").nonEmpty)
    assert(compileErrors("""val aInit = a init b"10101"""").nonEmpty)
    assert(compileErrors("""val aInit = a.init(b"1000", b"1001", b"10101")""").nonEmpty)
  }
  test("Initialization with special SameBitVectors b0s and b1s") {
    val aInit = (a init b0s).member.asInstanceOf[DFAny.Dcl]
    val aInitManual = (a init b"0000").member.asInstanceOf[DFAny.Dcl]
    assert(aInit.externalInit == aInitManual.externalInit)
    val bInit = (a init b1s).member.asInstanceOf[DFAny.Dcl]
    val bInitManual = (a init b"1111").member.asInstanceOf[DFAny.Dcl]
    assert(bInit.externalInit == bInitManual.externalInit)
    //sanity check
    assert(aInit.externalInit != bInit.externalInit)
  }
  test("A second Initialization attempt") {
    assert(compileErrors("""val aInit = a init b"1000"; val aInit2 = aInit init b"1001"""").nonEmpty)
  }
}
