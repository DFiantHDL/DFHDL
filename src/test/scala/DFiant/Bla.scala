package DFiant


trait Bla1 extends DFDesign {
  val i = DFBits(8) <> IN init b0s
  val o = DFBits(8) <> OUT
  val o2 = DFBool() <> OUT
  val temp = DFBits(8)
//  temp := b"11111111"
  val tempXor = (i ^ b"11111111").pipe()
  temp := tempXor
  o <> temp
  o2 <> tempXor.bit(0)
}


trait Mux1 extends DFDesign {
  val sel = DFBool() <> IN                                   //latency = Some(0)
  val a = DFBool() <> IN                                     //latency = Some(1)
  val b = DFBool() <> IN                                     //latency = Some(0)
  val res = DFBool() <> OUT                                  //latency = Some(0)
  res := ((sel && a) || (!sel && b)).pipe
}

trait Cont extends DFDesign {
  val sel = DFBool() <> IN                                   //latency = Some(0)
  val a = DFBool() <> IN                                     //latency = Some(1)
  val b = DFBool() <> IN                                     //latency = Some(0)
  val res = DFBool() <> OUT                                  //latency = Some(0)
  val mux = new Mux1 {}
  mux.sel <> sel.pipe()
  mux.a <> a
  mux.b <> b
  mux.res <> res

}
object Bla extends App {
  implicit val a = DFAnyConfiguration.foldedLatency
  val bla = new Cont {}.printCodeString
}
