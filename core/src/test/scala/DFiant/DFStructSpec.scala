import DFiant.*
import DFiant.core.SameBitsVector
import munit.*

class DFStructSpec extends DFSpec:
  case class Oron(x: DFBits[8] <> VAL, y: DFUInt[8] <> VAL) extends DFFields
  def test(t: core.DFValOf[core.DFStruct[Oron]]): Unit =
    t match
      case Oron(Zeros, y) if y == 22 =>
      case o: Oron                   =>

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {}
  test("Assignment") {}
end DFStructSpec
