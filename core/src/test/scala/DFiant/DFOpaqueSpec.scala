import DFiant.*
import munit.*

class DFOpaqueSpec extends DFSpec:
  assertCodeString(
    """|val xx = x <> VAR
       |val yy = y <> VAR
       |val u8 = DFUInt(8) <> VAR
       |val z = u8.as(x)
       |val zz = xx.actual
       |zz := d"8'15"
       |""".stripMargin
  ) {
    object x extends DFOpaque(DFUInt(8))
    object y extends DFOpaque(DFUInt(8))
//    val xx: x.type <> VAL = x <> VAR
//    val yy = y <> VAR
////    xx := yy
//    val u8 = DFUInt(8) <> VAR
//    val z = u8.as(x)
//    val zz = xx.actual
//    zz := 15
  }
  test("Inlined width") {
    object x extends DFOpaque(DFUInt(8))
//    x.width.verifyInlined(8)
  }

  test("Token Construction") {}
  test("Token Conversion") {}
  test("Comparison") {}
  test("Assignment") {}
end DFOpaqueSpec
