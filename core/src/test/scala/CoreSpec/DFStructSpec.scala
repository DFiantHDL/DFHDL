package CoreSpec
import dfhdl.*
import munit.*

class DFStructSpec extends DFSpec:
  class CCs[W <: Int](width: Inlined[W]):
    case class XY(x: Bits[W] <> VAL, y: UInt[W] <> VAL) extends Struct
    case class XYZ(x: UInt[W] <> VAL, y: Bits[W] <> VAL, z: Bit <> VAL) extends Struct

  case class VectorHolder(
      vec1: UInt[8] X 5 <> VAL,
      vec2: UInt[8] X 5 X 5 <> VAL
  ) extends Struct
  val cc = new CCs(8)
  import cc.{XY, XYZ}
  @inline def test(t: XY <> VAL): XY <> RET =
    t match
      case XY(all(0), y) if y == 22 => t
      case o: XY                    => t
  assertCodeString(
    """|val t1 = XY <> VAR init XY(x = h"05", y = d"8'1")
       |val pt1x = t1.x.prev
       |val t2 = XYZ <> VAR
       |t1.x := t2.y
       |t2.y := t1.x
       |t1 := XY(x = h"2a", y = d"8'0")
       |val t3 = t2 == XYZ(x = d"8'22", y = h"??", z = 1)
       |val t4 = VectorHolder <> VAR
       |t4.vec1(0) := d"8'22"
       |t4.vec2(4)(2) := d"8'25"
       |""".stripMargin
  ) {
    val t1 = XY <> VAR init XY(h"05", 1)
    t1.x.verifyValOf[Bits[8]]
    t1.y.verifyValOf[UInt[8]]
    val pt1x = t1.x.prev
    val t2 = XYZ <> VAR
    t1.x := t2.y
    t2.y := t1.x
    t1 := XY(h"2A", 0)
    val t3 = t2 == XYZ(22, ?, 1)
    val t4 = VectorHolder <> VAR
    t4.vec1(0) := 22
    t4.vec2(4)(2) := 25
  }

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {
    assertEquals(XY.token(XY(h"27", 1)).bits == h"2701", Boolean.token(true))
    assertEquals(h"2701".as(XY) != XY.token(XY(h"27", 1)), Boolean.token(false))
  }
  test("Assignment") {
    val cc2 = new CCs(8)
    val t1 = XY <> VAR
    val t2 = cc2.XY <> VAR
    t1 := t2
    val cc3 = new CCs(9)
    val t3 = cc3.XY <> VAR
    assertCompileError(
      "Mismatch structure value type `cc3.XY` for DFHDL receiver structure type `cc2.XY`."
    )(
      """t2 := t3"""
    )
    val eight = 8
    val cc4 = new CCs(eight)
    val t4 = cc4.XY <> VAR
    t1 := t4
    val nine = 9
    val cc5 = new CCs(nine)
    val t5 = cc5.XY <> VAR
    assertRuntimeError(
      """|Mismatch in structure fields.
         |The applied value type is:
         |DFStruct(XY,ListMap(x -> DFBits(9), y -> DFDecimal(false,9,0)))
         |The receiver type is:
         |DFStruct(XY,ListMap(x -> DFBits(8), y -> DFDecimal(false,8,0)))
         |""".stripMargin
    ) {
      t1 := t5
    }
  }

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
      case class XY(x: UInt[8] <> VAL, y: Bits[3] <> VAL) extends Struct
      // constructing the DFHDL value with initialization
      val xy = XY <> VAR init XY(x = 0, y = b"101")
      // accessing the fields and assigning them individually
      xy.x := 22
      xy.y := all(0)
      // assign the whole struct, with named fields
      xy := XY(x = 22, y = all(0))
      // assign the whole struct, as unnamed fields
      xy := XY(22, all(0))
      // easily composing structs
      case class XYZ(xy: XY <> VAL, z: Bit <> VAL) extends Struct
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
