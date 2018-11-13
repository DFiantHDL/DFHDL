package course

import DFiant._


case class Complex(w : Int) extends DFStruct {
  val r = DFSInt(w)
  val i = DFSInt(w)

  def + (that : Complex) : Complex = {
    val comp = Complex(w)
    comp.r := this.r + that.r
    comp.i := this.i + that.i
    comp
  }

  def * (that : Complex) : Complex = {
    val comp = Complex(w)
    comp.r := this.r * that.r - this.i * that.i
    comp.i := this.r * that.i + this.i * that.r
    comp
  }
}


