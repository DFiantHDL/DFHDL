package CoreSpec

import dfhdl.*
import munit.*

class ModifierSpec extends DFSpec:
  test("REG test") {
    class RegDsn extends RTDesign:
      val r = UInt(8) <> REG

    class ID extends DFDesign:

      val dmn = new RTDomain:
        val r = UInt(8) <> REG
  }
