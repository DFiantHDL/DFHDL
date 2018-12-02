package DFiant

object Bla extends App {
  import GlobalDesign._
  val u = DFUInt(7)
  val b = DFBits(8)

  val us = DFAny.Source(u)
  val bs = DFAny.Source(b)
//  val a = us.bitsWL(4, 0).reverse ## us.bitsWL(4, 4).reverse
  println(us.replaceWL(2, 4, bs.bitsWL(2,1)))

  val tkb = DFBits.Token(8, b"00001111")
  val tku = DFUInt.Token(8, 128)
  println(tku.replaceWL(4,0,tkb.bits(3,0)).bits)

  val tkn = Seq(DFBool.Token(1), DFBool.Token(1), DFBool.Token(Bubble), DFBool.Token(1))
  println(DFBool.Token.fromBits(DFBool.Token(Bubble).bits))

}
