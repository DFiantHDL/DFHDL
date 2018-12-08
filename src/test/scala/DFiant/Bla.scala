package DFiant


trait Bla1 extends DFDesign {
  val i = DFBits(8) <> IN init b0s
  val o = DFBits(8) <> OUT
  val temp = DFBits(8)
  temp := b"11111111"
  val tempXor = i ^ temp
  tempXor.pipe
  temp := tempXor
  o <> temp
}

object Bla extends App {
  implicit val a = DFAnyConfiguration.foldedLatency
  val bla = new Bla1 {}.printCodeString
}
