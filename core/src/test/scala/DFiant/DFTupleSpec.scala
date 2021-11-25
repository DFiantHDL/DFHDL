import DFiant.*
import munit.*
import compiletime.ops.int.>

class DFTupleSpec extends DFSpec:
  val tplA = (DFUInt(8), DFBit, DFBits(3))
  val tplB = ((DFUInt(8), DFBit), DFBits(3))
  val tokenA = tplA token (22, 1, b"101")
  val tokenB = tplB token ((22, 1), b"101")
  assertEquals(d"8'22", tokenA(0))
  assertEquals(d"8'22", tokenA._1)
//  assertEquals(tokenB.apply(0), DFBit.token(1))
  assertEquals(tokenB._1._1, d"8'22")
  assertCodeString(
    """|val x = (DFUInt(8), DFBit, DFBits(3)) <> VAR init ((d"8'0", 1, b"000"), (d"8'22", 0, b"101"))
       |val y = ((DFUInt(8), DFBit), DFBits(3)) <> VAR init ((d"8'11", 1), b"010")
       |""".stripMargin
  ) {
    val x: (DFUInt[8], DFBit, DFBits[3]) <> VAR =
      tplA <> VAR init ((0, 1, b0s), (22, 0, b"101"))
    val y: ((DFUInt[8], DFBit), DFBits[3]) <> VAR =
      tplB <> VAR init ((d"11", 1), b"010")
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
