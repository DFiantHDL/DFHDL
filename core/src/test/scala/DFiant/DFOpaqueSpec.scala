import DFiant.*
import munit.*

class DFOpaqueSpec extends DFSpec:
  val Pixel = DFOpaque((DFUInt(8), DFBit))
  val Pixel2 = DFOpaque(Pixel)
  assertCodeString(
    """|val xx = x <> VAR
       |val yy = y <> VAR
       |val u8 = DFUInt(8) <> VAR
       |val z = u8.as(x)
       |val zz = xx.actual
       |zz := d"8'15"
       |""".stripMargin
  ) {
    val x = DFOpaque(DFUInt(8))
    val y = DFOpaque(DFUInt(8))
    val xx = x <> VAR
    val yy = y <> VAR
//    xx := yy
    val u8 = DFUInt(8) <> VAR
    val z = u8.as(x)
    val zz = xx.actual
    zz := 15
  }
  test("Inlined width") {
    val x = DFOpaque(DFUInt(8))
    x.width.verifyInlined(8)
  }

  test("Token Construction") {}
  test("Token Conversion") {}
  test("Comparison") {}
  test("Assignment") {}
end DFOpaqueSpec
