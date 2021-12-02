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
  assertEquals(tokenB(0)(1), DFBit.token(1))
  assertEquals(tokenB._1._1, d"8'22")
  assertCodeString(
    """|val t1 = (DFUInt(8), DFBit, DFBits(3)) <> VAR init ((d"8'0", 1, b"000"), (d"8'22", 0, b"101"))
       |val t2 = ((DFUInt(8), DFBit), DFBits(3)) <> VAR init ((d"8'11", 1), b"010")
       |val t3 = ((DFUInt(8), DFBit), DFBits(3)) <> VAR init ((d"8'22", 1), b"101")
       |val t4 = ((DFUInt(8), DFBit), DFBits(3)) <> VAR init ((?, ?), b"???")
       |t3 := ((d"8'22", 1), b"101")
       |t3 := ((d"8'11", 1), b"010")
       |t4 := t3
       |val t5 = t3 == t4
       |val t6 = t3 == ((d"8'22", 1), b"101")
       |val b3 = DFBits(3) <> VAR
       |val t7 = t3 == ((d"8'11", 1), b3)
       |""".stripMargin
  ) {
    val t1: (DFUInt[8], DFBit, DFBits[3]) <> VAR =
      tplA <> VAR init ((0, 1, b0s), (22, 0, b"101"))
    val t2: ((DFUInt[8], DFBit), DFBits[3]) <> VAR =
      tplB <> VAR init ((d"11", 1), b"010")
    val t3 = tplB <> VAR init tokenB
    val t4 = tplB <> VAR init ?
    t3 := tokenB
    t3 := ((d"11", 1), b"010")
    t4 := t3
    val t5 = t3 == t4
    val t6 = t3 == tokenB
//    tokenA == (22, 1, b"101")
    val b3 = DFBits(3) <> VAR
    val t7 = t3 == ((d"8'11", 1), b3)
//    val t5: (DFUInt[8], DFBit, DFBits[3]) <> VAR =
//      tplA <> VAR init (22, 0, (b"1", b"0", b"0"))
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
