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

//  val mul32 = new Mul32 {}
//  val vhdMul32 = mul32.compileToVHDL.print().toFile("test.vhd")

  val mul32Seq = new Mul32Seq {}
  val vhdMul32Seq = mul32Seq.compileToVHDL.print().toFile("test.vhd")
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
  final val width  = 32
  final val a      = DFBits(width) <> IN
  final val b      = DFBits(width) <> IN
  final val tp     = DFBits(width) <> OUT
  final val prod   = DFBits(width) <> OUT

  tp := b0s
  for (i <- 0 until width) {
    val m = DFBits(width).selectdf(a(i))(b, b0s)
    val sum = (m.uint + tp.uint).wc
    prod(i) := sum.bits.lsbit
    tp := sum.bits.msbits(width)
  }
}


trait Mul32Seq extends DFDesign {
  final val width   = 32
  final val a       = DFBits(width) <> IN
  final val b       = DFBits(width) <> IN
  final val tp      = DFBits(width) <> OUT
  final val prod    = DFBits(width) <> OUT

  private val ai    = DFBits(width)
  private val bi    = DFBits(width)
  private val stage = DFUInt.rangeTo(width) init width

  ifdf (stage == width) {
    ai := a
    bi := b
    prod := b0s
    tp := b0s
    stage := 0
  }.elsedf {
    stage := stage + 1
  }
  private val m = DFBits(width).selectdf(ai(0))(bi, b0s)
  private val sum = (m.uint + tp.uint).wc
  prod := sum.bits(0,0) ## prod.msbits(width - 1)
  tp := sum.bits.msbits(width)
  ai := ai >> 1
}



