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
    """|???
       |""".stripMargin
  ) {
    val t1: DFStruct[XY] <> VAR = DFStruct[XY] <> VAR // init XY(Zeros, 1)
    t1.x.verifyTypeOf[DFBits[8] <> VAR]
    t1.y.verifyTypeOf[DFUInt[8] <> VAR]
    val t2: DFStruct[XYZ] <> VAR = DFStruct[XYZ] <> VAR
//    t1 := t1
//    val t4 = DFStruct[XYZ] <> VAR init ?
  }

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {}
  test("Assignment") {}
end DFStructSpec
