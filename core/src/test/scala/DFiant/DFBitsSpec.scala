package DFiant
import org.scalatest.flatspec.AnyFlatSpec
import DFiant.internals.Meta
import shapeless.test.illTyped

abstract class DFSpec extends AnyFlatSpec with DFDesign.Infra {
  private[DFiant] lazy val __ctx : DFDesign.Context = new DFBlock.Context(implicitly[Meta], null, new DFDesign.DB.Mutable)
}

class DFBitsSpec extends DFSpec {
  val one = 1
  val three = 3
  val four = 4
  val negOne = -1
  val a = DFBits(4)
  "Construction" should "support positive widths" in {
    assertCompiles("""val a : DFBits[4] = DFBits(4)""")
    assertCompiles("""val a : DFBits[4] = DFBits[4]""")
    assertCompiles("""val a : DFBits[Int] = DFBits(four)""")
  }
  it should "not support negative or zero widths" in {
    assertDoesNotCompile("""val a = DFBits[0]""")
    assertDoesNotCompile("""val a = DFBits[-1]""")
    assertThrows[IllegalArgumentException] {val a = DFBits(negOne)}
  }
  "Bit Range Selection" should "support index access within its width boundary" in {
    assertCompiles("""val b : DFBits[2] = a(1, 0)""")
    assertCompiles("""val b : DFBits[4] = a(3, 0)""")
    assertCompiles("""val b : DFBits[1] = a(2, 2)""")
    assertCompiles("""val b : DFBits[Int] = a(three, 2)""")
    assertCompiles("""val b : DFBits[Int] = a(3, three)""")
    assertCompiles("""val b : DFBits[Int] = a(three, one)""")
  }
  it should "not allow bit index access outside the boundary" in {
    assertDoesNotCompile("""val b = a(1, -1)""")
    assertDoesNotCompile("""val b = a(5, 0)""")
    assertThrows[IllegalArgumentException] {val b = a(1, negOne)}
    assertThrows[IllegalArgumentException] {val b = a(four, 1)}
  }
  it should "not allow low index to be higher than the high index" in {
    assertDoesNotCompile("""val b = a(1, 2)""")
    assertThrows[IllegalArgumentException] {val b = a(1, three)}
  }
  "Bit Selection" should "support index access within its width boundary" in {
    assertCompiles("""val b : DFBit = a(0)""")
    assertCompiles("""val b : DFBit = a(3)""")
    assertCompiles("""val b : DFBit = a(three)""")
  }
  it should "not allow bit index access outside the boundary" in {
    assertDoesNotCompile("""val b = a(-1)""")
    assertDoesNotCompile("""val b = a(5)""")
    assertThrows[IllegalArgumentException] {val b = a(negOne)}
    assertThrows[IllegalArgumentException] {val b = a(four)}
  }
  "Initialization" should "support BitVectors" in {
    assertCompiles("""val aInit = a init b"1000"""")
    assertCompiles("""val aInit = a init h"F"""")
    assertCompiles("""val aInit = a init(h"1", h"2", h"F")""")
  }
  it should "not allow vectors with different lengths than the constructed variable" in {
    val i = b"101"
    assertDoesNotCompile("""val aInit = a init b"1"""")
    assertDoesNotCompile("""val aInit = a init b"10101"""")
    assertDoesNotCompile("""val aInit = a init(b"1000", b"1001", b"10101")""")
  }
  it should "support special SameBitVectors b0s and b1s" in {
    val aInit = a init b0s
    val aInitManual = a init b"0000"
    assert(aInit.asInstanceOf[DFAny.Dcl].externalInit == aInitManual.asInstanceOf[DFAny.Dcl].externalInit)
    val bInit = a init b1s
    val bInitManual = a init b"1111"
    assert(bInit.asInstanceOf[DFAny.Dcl].externalInit == bInitManual.asInstanceOf[DFAny.Dcl].externalInit)
    //sanity check
    assert(aInit.asInstanceOf[DFAny.Dcl].externalInit != bInit.asInstanceOf[DFAny.Dcl].externalInit)
  }
  it should "not be possible to apply twice" in {
    assertDoesNotCompile("""val aInit = a init b"1000"; val aInit2 = aInit init b"1001"""")
  }


}
