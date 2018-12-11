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
  res <> ((sel && a) || (!sel && b))
}

trait MuxN extends DFDesign {
  final val n = Cont.n
  final val sel = DFBool() <> IN
  final val a   = DFBits(n) <> IN
  final val b   = DFBits(n) <> IN
  final val res = DFBits(n) <> OUT
  for (i <- 0 until n) {
    val mux = new Mux1 {}.setName(s"m$i")
    mux.sel <> sel
    mux.a <> a(i)
    mux.b <> b(i)
    res(i) := mux.res
  }
}

trait Bug1 extends DFDesign {
  final val n = 32
  final val a   = DFBits(n) <> IN
  final val res = DFBits(n) <> OUT
  val tempL = DFBits(16)
  val tempR = DFBits(16)
  tempL := a(31, 16)
  tempR := a(15, 0)
  (tempL, tempR).bits.bits(23,16) := h"57"
  res(31, 16) := tempL
  res(15, 0) := tempR
}

trait Cont extends DFDesign {
  val selP = DFBool() <> IN                                   //latency = Some(0)
  val aP = DFBool() <> IN                                     //latency = Some(1)
  val bP = DFBool() <> IN                                     //latency = Some(0)
  val resP = DFBool() <> OUT                                  //latency = Some(0)
  val mux = new Mux1 {}.showConnections
  mux.sel <> selP
  mux.a <> aP
  mux.b <> bP
  resP := mux.res

//  final val n = Cont.n
//  final val a   = DFUInt(n) <> IN
//  final val res = DFUInt(n) <> OUT
//  val temp = DFUInt(32) init 5
//  temp := a
//  val tp = temp.prev
//  res := a + 1
}

object Cont {
  final val n = 32
}

object Bla extends App {
  val bla = new Mux1 {}.showConnections.printCodeString
}
