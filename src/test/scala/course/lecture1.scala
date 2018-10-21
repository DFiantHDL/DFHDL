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



trait Add2 extends DFDesign {
  final val a     = DFBits(2) <> IN
  final val b     = DFBits(2) <> IN
  final val c_in  = DFBool()  <> IN
  final val s     = DFBits(2) <> OUT
  final val c_out = DFBool()  <> OUT

  val fa0 = new FA {}
  val fa1 = new FA {}

  fa0.a <> a.bit(0)
  fa0.b <> b.bit(0)
  fa0.c_in <> c_in

  fa0.c_out <> fa1.c_in

  fa1.a <> a.bit(1)
  fa1.b <> b.bit(1)
  fa1.c_out <> c_out
}


object TestFA extends App {
  //  val fa = new FA {}.printCodeString
  //  val fa = new FA {}.printVHDLString

//  val add2 = new Add2 {}.printCodeString
  val add2 = new Add2 {}.printVHDLString

}