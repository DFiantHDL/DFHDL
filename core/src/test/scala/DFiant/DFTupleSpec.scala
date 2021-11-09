import DFiant.*
import munit.*

class DFTupleSpec extends DFSpec:
  val tplA = (DFUInt(8), DFBit, DFBits(3))
  val tplB = ((DFUInt(8), DFBit), DFBits(3))
  assertCodeString(
    """|val x = (DFUInt(8), DFBit, DFBits(3)) <> VAR
       |val y = ((DFUInt(8), DFBit), DFBits(3)) <> VAR
       |""".stripMargin
  ) {
    val x: (DFUInt[8], DFBit, DFBits[3]) <> VAR =
      tplA <> VAR init (0, 1, b0s)
    val y: ((DFUInt[8], DFBit), DFBits[3]) <> VAR = tplB <> VAR
  }

  test("Inlined width") {
    tplA.width.verifyInlined(12)
    tplB.width.verifyInlined(12)
  }

  test("Token Construction") {
//    val t1: o1u8.type <> TOKEN = 1.as(o1u8)
//    val t2: o2u8.type <> TOKEN = d"22".as(o2u8)
//    t1 == t1
  }
  test("Comparison") {}
  test("Assignment") {}
end DFTupleSpec
