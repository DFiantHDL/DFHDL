import DFiant.*
import munit.*

class DFTupleSpec extends DFSpec:
  val x = (DFUInt(8), DFBit, DFBits(3))
  val y = ((DFUInt(8), DFBit), DFBits(3))
  test("Inlined width") {
    x.width.verifyInlined(12)
    y.width.verifyInlined(12)
  }

  test("Token Construction") {
//    val t1: o1u8.type <> TOKEN = 1.as(o1u8)
//    val t2: o2u8.type <> TOKEN = d"22".as(o2u8)
//    t1 == t1
  }
  test("Comparison") {}
  test("Assignment") {}
end DFTupleSpec
