package DFiant

object Bla extends App {
  import GlobalDesign._
  val u = DFUInt(8)
  val b = DFBits(8)

  val us = DFAny.Source(u)
  val a = us.bitsWL(4, 0).reverse ## us.bitsWL(4, 4).reverse
  println(a)

}
