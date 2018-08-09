import DFiant._
import DFiant.internals._

import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
new DFDesign {
  val a = b"1111111"
  val b = DFBits(8)

  a & b
}

h"abcd"