import DFiant._

import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
val d = new DFDesign {
  val a : DFUInt[8] <> IN = TOP
  val b : DFUInt[8] <> IN = TOP
  val o : DFUInt[8] <> OUT = TOP

  val oo = DFUInt[8]
  val aa = DFUInt[8]
  o := aa
}

//d.mama.getName
