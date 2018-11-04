package course
import DFiant._


object TestFA extends App {
//    implicit val a = DFAnyConfiguration.detailed
//    val fa = new FA {}.printCodeString
//    val fa = new FA {}.printVHDLString

//    val add2 = new Add2 {}.printCodeString
//    val add2 = new Add2 {}.printVHDLString

//    val addN = new AddN(8) {}.printCodeString
//    val addN = new AddN(8) {}
//    val vhdAddN = addN.compileToVHDL.print().toFile("test.vhd")

  val mul32 = new Mul32 {}
  val vhdMul32 = mul32.compileToVHDL.print().toFile("test.vhd")
}







































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

  private val fa0 = new FA {}
  private val fa1 = new FA {}

  fa0.a <> a(0)
  fa0.b <> b(0)
  s(0) := fa0.s
  fa0.c_in <> c_in

  fa0.c_out <> fa1.c_in

  fa1.a <> a(1)
  fa1.b <> b(1)
  s(1) := fa1.s
  fa1.c_out <> c_out
}

class AddN(n : Int)(implicit ctx : DFDesign.ContextOf[AddN]) extends DFDesign {
  final val a     = DFBits(n) <> IN
  final val b     = DFBits(n) <> IN
  final val c_in  = DFBool()  <> IN
  final val s     = DFBits(n) <> OUT
  final val c_out = DFBool()  <> OUT

  private val fa = Array.fill(n)(new FA {})

  fa.head.c_in <> c_in
  for (i <- 0 until n) {
    val faCur = fa(i).setName(s"fa$i")
    faCur.a <> a(i)
    faCur.b <> b(i)
    s(i) := faCur.s
    if (i < n - 1)
      faCur.c_out <> fa(i+1).c_in
  }
  fa.last.c_out <> c_out
}


class MyCounter(w : Int) extends DFDesign {
  final val en  = DFBool() <> IN
  final val out = DFUInt(w) <> OUT init(0)
  final val isZero = DFBool() <> OUT
  ifdf (en) {out := out + 1}
  isZero := out.isZero
}
object Top extends DFDesign {
  val out = DFUInt(8) <> OUT
  val c8 = new MyCounter(8) {}
  c8.en <> true
  c8.out <> out
}


trait Mul32 extends DFDesign {

  final val a     = DFBits(32) <> IN
  final val b     = DFBits(32) <> IN
  final val tp    = DFBits(32) <> OUT
  final val prod  = DFBits(32) <> OUT

  tp := b0s
  for (i <- 0 until 32) {
    val sel = a(i).setName(s"sel$i")
    val m = DFBits(32).selectdf(sel)(b, b0s).setName(s"m$i")
    val sum = (m.uint + tp.uint).wc
    prod(i) := sum.bits.lsbit
    tp := sum.bits.msbits(32)
  }
}


