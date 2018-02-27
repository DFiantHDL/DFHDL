package DFiant

import DFiant.internals._

abstract class OldDFDesign[Top] {
  implicit val design : OldDFDesign[Top] = this

  protected def top_builder() : Top
  final val top : Top = top_builder()

  def simulate(iterations : Int) : Unit = {
    for (i <- 0 until iterations) {
//      Almanac.newSimPhase()
      top_builder()
    }
  }

  def compileToVerilog() : Unit = ???
  def compileToVHDL() : Unit = ???
}


//object DFDesign {
//  def apply(dfCode: => Unit) : DFDesign = new DFDesign(dfCode)
//}

//abstract class Boxy {
//  val in1   : DFBits#IN
//  val in2   : DFBits#IN
//  val out1  : DFBits#OUT
//  val out2  : DFBits#OUT
//
//  out1 := in1 + in2
//  out2 := in1 - in2
//}

//object testy {
//  val myDesign = new DFDesign[Boxy] {
//    val a = DFBits(8)
//    val b = DFBits(8)
//    val r1 = DFBits(8)
//    val r2 = DFBits(8)
//
//    protected def top_builder() = {
//      new Boxy {
//        val in1   : DFBits#IN   = a
//        val in2   : DFBits#IN   = b
//        val out1  : DFBits#OUT  = r1
//        val out2  : DFBits#OUT  = r2
//      }
//    }
//    a.simInject(1)
//    b.simInject(2)
//    r1.simWatch
//    a.simInject(5)
//    a.simInject(6)
//    a.simInject(7)
//  }
//
//  myDesign.simulate(3)
//
//}