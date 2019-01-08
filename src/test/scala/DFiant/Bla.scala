package DFiant


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

//class IMem()(implicit ctx : RTComponent.Context) extends RTComponent {
//  final val clka = Clock()
//  final val addra = DFBits(12) <> IN
//  final val douta = DFBits(32) <> OUT
////  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
//}

trait Inst extends DFDesign {
  val g_predict = DFBool() <> OUT
  val l_predict = DFBool() <> OUT
  final val wea = DFBool() <> IN
  g_predict := wea
  l_predict := wea
}


trait Cont extends DFSimulator {
//  final val addraP = DFBits(12) <> IN
//  final val doutaP = DFBits(32) <> OUT
////  val imem = new IMem()
////  imem.addra <> addraP
////  imem.douta <> doutaP
//  matchdf(addraP)
//    .casedf(b"11111111111") {doutaP := b0s}
//    .casedf(b"01111111111") {}
//    .casedf_{doutaP := b1s}
  val i = DFBool() <> IN
  val o = DFBool() <> OUT
  val inst = new Inst {}
  private val selector = DFBool()
  private val g_predict = DFBool()
  private val l_predict = DFBool()

  selector := i
  o := DFBool().selectdf(selector)(l_predict,g_predict)
//  val cc = DFUInt(8) <> OUT
//  val cnt = DFUInt(8)
//  sim.report(msg"$cnt")
//  cc := cnt
}


object Bla extends App {
  val bla = new Cont {}.compileToVHDL.print().toFile("tour.vhd")
}
