package CoreSpec
import dfhdl.*
import munit.*

class DFUnitSpec extends DFSpec:
  test("DFHDL Unit as bottom type") {
    assertCodeString("val x = Bit <> VAR") {
      val x = Bit <> VAR
      val y: Unit <> VAL = !x
      val y2: Unit <> VAL = (x, !x)
    }
  }
end DFUnitSpec
