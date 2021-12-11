import DFiant.*
import munit.*

class DFStructSpec extends DFSpec:
  case class Oron(x: DFBits[8] <> VAL, y: DFUInt[8] <> VAL) extends DFFields
  def test(t: core.DFStruct[Oron]): Unit =
    t match
      case Oron(_, 111) =>

  test("Inlined width") {}
  test("Token Construction") {}
  test("Comparison") {}
  test("Assignment") {}
end DFStructSpec
