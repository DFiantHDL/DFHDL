package DFiant

object BasicTest extends App {
  trait MyDesign extends DFDesign {
    type W = 8
    val a_in : DFUInt[W] <> IN = TOP
    val b_in : DFUInt[W] <> IN = TOP
    val c_out : DFUInt[W] <> OUT = TOP
    c_out := a_in + b_in
  }

  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  val myDesign = new MyDesign {}
  myDesign.compileToVHDL("myDesignTest")
}
