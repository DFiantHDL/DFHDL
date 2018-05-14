package DFiant

object BasicTest extends App {

  trait MemeDesign extends DFDesign {
    type W = 8
    val meme = new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
    }
    new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
    }
  }

  trait MyDesign extends MemeDesign {
    val a_in : DFUInt[W] <> IN = TOP
    val b_in : DFUInt[W] <> IN = TOP
    val c_out : DFUInt[W] <> OUT = TOP
    c_out := a_in + b_in
  }

  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  val myDesign = new MyDesign {}
//  myDesign.meme.keep
//  myDesign.wakeLazyAlmanac
//  myDesign.protAlmanac.printEntrees()
//  println(myDesign.meme.getName)
//  myDesign.keep
//  println(myDesign.components)

  myDesign.printInfo()

//  myDesign.compileToVHDL("myDesignTest")
}
