package DFiant

object Bla extends App {
  import GlobalDesign._
  val u = DFUInt(7)
  val b = DFBits(8)

  val us = DFAny.Source(u)
  val bs = DFAny.Source(b)
//  val a = us.bitsWL(4, 0).reverse ## us.bitsWL(4, 4).reverse
  println(us.replaceWL(2, 4, bs.bitsWL(2,1)))

}
