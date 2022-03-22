package CoreSpec
import DFiant.*
import munit.*

class DFStructSpec extends DFSpec:
  class CCs[W <: Int](width: Inlined[W]):
    case class XY(x: DFBits[W] <> VAL, y: DFUInt[W] <> VAL) extends DFStruct
    case class XYZ(x: DFUInt[W] <> VAL, y: DFBits[W] <> VAL, z: DFBit <> VAL) extends DFStruct

  val cc = new CCs(8)
  import cc.{XY, XYZ}
  def test(t: XY <> VAL): Unit =
    t match
      case XY(all(0), y) if y == 22 =>
      case o: XY                    =>
  assertCodeString(
    """|val t1 = XY <> VAR init XY(x = h"05", y = d"8'1")
       |val pt1x = t1.x.prev
       |val t2 = XYZ <> VAR
       |t1.x := t2.y
       |t2.y := t1.x
       |t1 := XY(x = h"2a", y = d"8'0")
       |val t3 = t2 == XYZ(x = d"8'22", y = h"??", z = 1)
       |""".stripMargin
  ) {
    val t1 = XY <> VAR init XY(h"05", 1)
    t1.x.verifyTypeOf[DFBits[8] <> VAL]
    t1.y.verifyTypeOf[DFUInt[8] <> VAL]
    val pt1x = t1.x.prev
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
  test("Assignment") {
    val cc2 = new CCs(8)
    val t1 = XY <> VAR
    val t2 = cc2.XY <> VAR
    t1 := t2
    val cc3 = new CCs(9)
    val t3 = cc3.XY <> VAR
    assertCompileError(
      "Mismatch structure value type `cc3.XY` for dataflow receiver structure type `cc2.XY`."
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
      case class XY(x: DFUInt[8] <> VAL, y: DFBits[3] <> VAL) extends DFStruct
      // constructing the dataflow value with initialization
      val xy = XY <> VAR init XY(x = 0, y = b"101")
      // accessing the fields and assigning them individually
      xy.x := 22
      xy.y := all(0)
      // assign the whole struct, with named fields
      xy := XY(x = 22, y = all(0))
      // assign the whole struct, as unnamed fields
      xy := XY(22, all(0))
      // easily composing structs
      case class XYZ(xy: XY <> VAL, z: DFBit <> VAL) extends DFStruct
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
