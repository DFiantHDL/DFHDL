import DFiant._

import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._

implicit val config = DFAnyConfiguration.detailed
val n = new DFDesign {
//  val a = (DFUInt(8) init 1, DFUInt(8) init 0).bits().keep
  val b = (DFUInt(8).asInstanceOf[DFUInt[8]], DFUInt(2)).bits().keep

}

n.codeString