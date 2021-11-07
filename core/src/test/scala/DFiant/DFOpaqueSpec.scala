import DFiant.*
import munit.*

class DFOpaqueSpec extends DFSpec:
  object o1u8 extends DFOpaque(DFUInt(8))
  object o2u8 extends DFOpaque(DFUInt(8))
  val o1 = o1u8 <> VAR init 1.as(o1u8)
  val o2 = o2u8 <> VAR
  object gogo extends DFOpaque((DFUInt(8), DFBit))
  assertCodeString(
    """|val o11 = o1u8 <> VAR init d"8'1".as(o1u8)
       |val u8 = DFUInt(8) <> VAR
       |val momo = (u8, 1).as(gogo)
       |val q = o1 == o1
       |val q2 = o1 == d"8'1".as(o1u8)
       |o1 := d"8'1".as(o1u8)
       |val z = u8.as(o1u8)
       |val zz = o1.actual
       |o2.actual := d"8'0"
       |zz := d"8'15"
       |""".stripMargin
  ) {
    val o11 = o1u8 <> VAR init 1.as(o1u8)
    val u8 = DFUInt(8) <> VAR
    val momo = (u8, 1).as(gogo)
    val q = o1 == o1
    val q2 = o1 == 1.as(o1u8)
    o1 := 1.as(o1u8)
    val z = u8.as(o1u8)
    val zz = o1.actual
    o2.actual := 0
    zz := 15
  }
  test("Inlined width") {
    object y extends DFOpaque((o1u8, DFBit))
    o1u8.width.verifyInlined(8)
    y.width.verifyInlined(9)
  }

  test("Token Construction") {
    val t1: o1u8.type <> TOKEN = 1.as(o1u8)
    val t2: o2u8.type <> TOKEN = d"22".as(o2u8)
    t1 == t1
  }
  test("Comparison") {
    assertCompileError(
      "Cannot compare dataflow value of type `DFOpaqueSpec.this.o1u8.type` with value of type `DFOpaqueSpec.this.o2u8.type <> VAR`."
    )(
      """o1 == o2"""
    )
  }
  test("Assignment") {
    assertCompileError(
      "Unsupported value of type `DFOpaqueSpec.this.o2u8.type <> VAR` for dataflow receiver type `DFOpaqueSpec.this.o1u8.type`."
    )(
      """o1 := o2"""
    )
  }
end DFOpaqueSpec
