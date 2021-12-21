import DFiant.*
import DFiant.core.SameBitsVector
import munit.*

class DFStructSpec extends DFSpec:
  case class XY(x: DFBits[8] <> VAL, y: DFUInt[8] <> VAL)
  case class XYZ(x: DFUInt[8] <> VAL, y: DFBits[8] <> VAL, z: DFBit <> VAL)
//  def test(t: core.DFValOf[core.DFStruct[Oron]]): Unit =
//    t match
//      case Oron(Zeros, y) if y == 22 =>
//      case o: Oron                   =>
  assertCodeString(
    """|val t1 = XY <> VAR
       |val t2 = XYZ <> VAR
       |t1.x := t2.y
       |t2.y := t1.x
       |""".stripMargin
  ) {
    val t1 = DFStruct[XY] <> VAR // init XY(Zeros, 1)
    t1.x.verifyTypeOf[DFBits[8] <> VAR]
    t1.y.verifyTypeOf[DFUInt[8] <> VAR]
    val t2 = DFStruct[XYZ] <> VAR
    t1.x := t2.y
    t2.y := t1.x
//    val t4 = DFStruct[XYZ] <> VAR init ?
  }

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {}
  test("Assignment") {}
end DFStructSpec
