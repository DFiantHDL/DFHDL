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
    object gogo extends DFOpaque((DFUInt(8), DFBit))
    val xx = x <> VAR
    val yy = y <> VAR
//    xx := yy
    val u8 = DFUInt(8) <> VAR
//    val momo = (u8, 1).as(gogo)
//    val z = u8.as(x)
//    val zz = xx.actual
//    zz := 15
  }
  test("Inlined width") {
    object x extends DFOpaque(DFUInt(8))
    object y extends DFOpaque((x, DFBit))
    x.width.verifyInlined(8)
    y.width.verifyInlined(9)
  }

  test("Token Construction") {}
  test("Token Conversion") {}
  test("Comparison") {}
  test("Assignment") {}
end DFOpaqueSpec
