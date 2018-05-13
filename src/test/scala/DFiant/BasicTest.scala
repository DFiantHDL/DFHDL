package DFiant
import basiclib._

object BasicTest extends App {

  abstract class MemeDesign(implicit basicLib: DFBasicLib) extends DFDesign {
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
  println(myDesign.a_in.getName)
  println(myDesign.getName)
  println(myDesign.meme.getName)
  println(myDesign.components)

  abstract class Foo(implicit n : sourcecode.Enclosing) {
    def getName = n.value
  }

  val nice, nice2 = new Foo {}

  println(nice.getName)
  println(nice2.getName)
//  myDesign.compileToVHDL("myDesignTest")
}
