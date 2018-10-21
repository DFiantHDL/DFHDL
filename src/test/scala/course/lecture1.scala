package course
import DFiant._

trait FA extends DFDesign {
  final val a     = DFBool() <> IN
  final val b     = DFBool() <> IN
  final val c_in  = DFBool() <> IN
  final val s     = DFBool() <> OUT
  final val c_out = DFBool() <> OUT

  s     <> ((a ^ b) ^ c_in)
  c_out <> ((a && b) || (c_in && (a ^ b)))
}

object TestFA extends App {
//  val fa = new FA {}.printCodeString
  val fa = new FA {}.printVHDLString


}
