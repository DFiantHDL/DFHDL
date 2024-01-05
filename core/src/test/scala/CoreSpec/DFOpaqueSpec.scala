package CoreSpec
import dfhdl.*
import munit.*

class DFOpaqueSpec extends DFSpec:
  case class o1u8() extends core.DFOpaque.Frontend(UInt(8))
  case class o2u8() extends Opaque(UInt(8))
  val o1 = o1u8 <> VAR init 1.as(o1u8)
  val o2 = o2u8 <> VAR
  case class gogo() extends Opaque((UInt(8), Bit))
  abstract class arrAbs extends Opaque(UInt(8) X 4)
  case class arr() extends arrAbs
  extension (a: arrAbs <> VAL) @inline def booAbs: arrAbs <> DFRET = a
  extension (a: arr <> VAL) @inline def boo: arr <> DFRET = a
  assertCodeString(
    """|val o11 = o1u8 <> VAR init d"8'1".as(o1u8)
       |val a = arr <> VAR init all(d"8'0").as(arr)
       |val u8 = UInt(8) <> VAR
       |val momo = (u8, 1).as(gogo)
       |val q = o1 == o1
       |val q2 = o1 == d"8'1".as(o1u8)
       |o1 := d"8'1".as(o1u8)
       |val z = u8.as(o1u8)
       |a := Vector(a.actual(0), a.actual(1), a.actual(2), a.actual(3)).as(arr)
       |val ax = all(u8).as(arr)
       |val conv = o1u8 CONST d"8'1".as(o1u8)
       |val zz = o1.actual
       |o2.actual := d"8'0"
       |zz := d"8'15"
       |""".stripMargin
  ) {
    val o11 = o1u8 <> VAR init 1.as(o1u8)
    val a = arr <> VAR init all(0).as(arr)
    a.booAbs
    a.boo
    val u8 = UInt(8) <> VAR
    val momo = (u8, 1).as(gogo)
    val q = o1 == o1
    val q2 = o1 == 1.as(o1u8)
    o1 := 1.as(o1u8)
    val z = u8.as(o1u8)
    a := a.actual.elements.as(arr)
    val x: Vector[UInt[8] <> VAL] = Vector.fill(4)(u8)
    val ax = x.as(arr)
    val conv: o1u8 <> VAL = 1.as(o1u8)
    val zz = o1.actual
    o2.actual := 0
    zz := 15
  }
  test("Inlined width") {
    case class y() extends Opaque((o1u8, Bit))
    o1u8.width.verifyInlined(8)
    y.width.verifyInlined(9)
  }

  test("Token Construction") {
    val t1: o1u8 <> TOKEN = 1.as(o1u8)
    val t2: o2u8 <> TOKEN = d"22".as(o2u8)
    t1 == t1
    assert(t1.actual == 1)
    assert(t2.actual == d"8'22")
  }
  test("Comparison") {
    assertCompileError(
      "Cannot compare DFHDL value of type `DFOpaqueSpec.this.o1u8` with value of type `DFOpaqueSpec.this.o2u8 <> VAR`."
    )(
      """o1 == o2"""
    )
  }
  test("Assignment") {
    assertCompileError(
      "Unsupported value of type `DFOpaqueSpec.this.o2u8 <> VAR` for DFHDL receiver type `DFOpaqueSpec.this.o1u8`."
    )(
      """o1 := o2"""
    )
  }
end DFOpaqueSpec
