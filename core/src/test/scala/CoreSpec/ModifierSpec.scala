package CoreSpec

import DFiant.*
import munit.*

class ModifierSpec extends DFSpec:
  test("REG test") {
    class RegDsn extends RTDesign:
      val r = DFUInt(8) <> REG

    class ID extends DFDesign:

      val dmn = new RTDomain:
        val r = DFUInt(8) <> REG
  }
