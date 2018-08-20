import DFiant._
import DFiant.internals._
implicit val a = DFAnyConfiguration.detailed
import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
trait MyDesign extends DFDesign{
  val in = DFSInt(4) <> IN init -1
  val out = DFUInt(4) <> OUT

  out <> in.uint
}

val myDesign = new MyDesign {}
println(myDesign.codeString)
