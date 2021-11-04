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
    val x = DFUInt(8).opaque
    val y = DFUInt(8).opaque
    val xx = x <> VAR
    val yy = y <> VAR
    val u8 = DFUInt(8) <> VAR
    val z = u8.as(x)
    val zz = xx.actual
    zz := 15
  }
  test("Inlined width") {
    val x = DFUInt(8).opaque
    x.width.verifyInlined(8)
  }

  test("Token Construction") {}
  test("Token Conversion") {}
  test("Comparison") {}
  test("Assignment") {}
end DFOpaqueSpec
