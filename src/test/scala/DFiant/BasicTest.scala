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
  myDesign.meme.keep
  println(myDesign.meme.getName)
//  myDesign.keep
  println(myDesign.components)
  myDesign.protAlmanac.printComponents()

  abstract class Foo(implicit n : sourcecode.Name) {
    def getName = n.value
  }

  val nice, nice2 = new Foo {}

  println(nice.getName)
  println(nice2.getName)
//  myDesign.compileToVHDL("myDesignTest")
}
