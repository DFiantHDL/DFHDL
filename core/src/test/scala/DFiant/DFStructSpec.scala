import DFiant.*
import DFiant.core.SameBitsVector
import munit.*

class DFStructSpec extends DFSpec:
  class CCs[W <: Int]:
    case class XY(x: DFBits[W] <> VAL, y: DFUInt[W] <> VAL)
    case class XYZ(x: DFUInt[W] <> VAL, y: DFBits[W] <> VAL, z: DFBit <> VAL)

  val cc = new CCs[8]
  import cc.{XY, XYZ}
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
    val t1 = XY <> VAR // init XY(Zeros, 1)
    t1.x.verifyTypeOf[DFBits[8] <> VAL]
    t1.y.verifyTypeOf[DFUInt[8] <> VAL]
    val t2 = XYZ <> VAR
    t1.x := t2.y
    t2.y := t1.x
//    val t4 = DFStruct[XYZ] <> VAR init ?
  }

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {}
  test("Assignment") {}
end DFStructSpec
