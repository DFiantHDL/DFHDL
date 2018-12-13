package course.Lab2

import DFiant._

trait Mux1 extends DFDesign {
  final val sel = DFBool() <> IN
  final val a   = DFBool() <> IN
  final val b   = DFBool() <> IN
  final val res = DFBool() <> OUT
  res := ((sel && a) || (!sel && b))
}

trait MuxN extends DFDesign {
  val w : Int
  final val sel = DFBool() <> IN
  final val a   = DFBits(w) <> IN
  final val b   = DFBits(w) <> IN
  final val res = DFBits(w) <> OUT
  for (i <- 0 until w) {
    val mux = new Mux1 {}.setName(s"m$i")
    mux.sel <> sel
    mux.a <> a(i)
    mux.b <> b(i)
    res(i) := mux.res
  }
}

object Mux {
  def apply(width : Int)(sel : DFBool)(trueVal : DFBits[Int], falseVal : DFBits[Int])(
    implicit ctx : DFDesign.Context
  ) : DFBits[Int] = {
    val mux = new MuxN {override lazy val w : Int = width}.setName(ctx.getName)
    mux.sel <> sel
    mux.a <> trueVal
    mux.b <> falseVal
    mux.res
  }
}




