package DFiant
import org.scalatest.flatspec.AnyFlatSpec
import DFiant.internals.Meta
import shapeless.test.illTyped

abstract class DFSpec extends AnyFlatSpec with DFDesign.Infra {
  private[DFiant] lazy val __ctx : DFDesign.Context = new DFBlock.Context(implicitly[Meta], null, new DFDesign.DB.Mutable)
}

class DFBitsSpec extends DFSpec {
  val four = 4
  val negOne = -1
  "DFBits construction" should "support positive widths" in {
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
    import singleton.twoface._
    val a = DFBits(4)
    assertCompiles("""val b : DFBits[2] = a(1, 0)""")
    assertCompiles("""val b : DFBits[4] = a(3, 0)""")
    assertCompiles("""val b : DFBits[1] = a(2, 2)""")
  }
}
