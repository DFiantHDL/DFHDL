import DFiant.*
import DFiant.core.SameBitsVector
import munit.*

class DFStructSpec extends DFSpec:
  class CCs[W <: Int]:
    case class XY(x: DFBits[W] <> VAL, y: DFUInt[W] <> VAL)
    case class XYZ(x: DFUInt[W] <> VAL, y: DFBits[W] <> VAL, z: DFBit <> VAL)

  val cc = new CCs[8]
  import cc.{XY, XYZ}
  def test(t: XY <> VAL): Unit =
    t match
      case XY(Zeros, y) if y == 22 =>
      case o: XY                   =>
  assertCodeString(
    """|val t1 = XY <> VAR init XY(x = h"8'05", y = d"8'1")
       |val t2 = XYZ <> VAR
       |t1.x := t2.y
       |t2.y := t1.x
       |t1 := XY(x = h"8'2a", y = d"8'0")
       |val t3 = t2 == XYZ(x = d"8'22", y = h"8'??", z = 1)
       |""".stripMargin
  ) {
    val t1 = XY <> VAR init XY(h"05", 1)
    t1.x.verifyTypeOf[DFBits[8] <> VAL]
    t1.y.verifyTypeOf[DFUInt[8] <> VAL]
    val t2 = XYZ <> VAR
    t1.x := t2.y
    t2.y := t1.x
    t1 := XY(h"2A", 0)
    val t3 = t2 == XYZ(22, ?, 1)
  }

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {
    assertEquals(XY.token(XY(h"27", 1)).bits == h"2701", DFBool.token(true))
    assertEquals(h"2701".as(XY) != XY.token(XY(h"27", 1)), DFBool.token(false))
  }
  test("Assignment") {}

  test("Showcase") {
    assertCodeString(
      """|val xy = XY <> VAR init XY(x = d"8'0", y = b"101")
         |xy.x := d"8'22"
         |xy.y := b"000"
         |xy := XY(x = d"8'22", y = b"000")
         |xy := XY(x = d"8'22", y = b"000")
         |val xyz = XYZ <> VAR
         |xyz.xy := xy
         |xyz.xy.x := d"8'55"
         |val result: XY <> VAL =
         |  if (xyz.z) xyz.xy
         |  else xy
         |""".stripMargin
    ) {
      // define the struct class
      case class XY(x: DFUInt[8] <> VAL, y: DFBits[3] <> VAL)
      // constructing the dataflow value with initialization
      val xy = XY <> VAR init XY(x = 0, y = b"101")
      // accessing the fields and assigning them individually
      xy.x := 22
      xy.y := Zeros
      // assign the whole struct, with named fields
      xy := XY(x = 22, y = Zeros)
      // assign the whole struct, as unnamed fields
      xy := XY(22, Zeros)
      // easily composing structs
      case class XYZ(xy: XY <> VAL, z: DFBit <> VAL)
      val xyz = XYZ <> VAR
      // field assignments of a whole struct
      xyz.xy := xy
      xyz.xy.x := 55
      // structures can also be muxed using an if expression
      val result: XY <> VAL =
        if (xyz.z) xyz.xy else xy
    }
  }
end DFStructSpec
