import DFiant.*

class Foo(using DFC) extends DFDesign:
  val x = DFUInt(8) <> IN
  val y = DFBits(8) <> OUT
  y := x.bits
//    .asInstanceOf[core.DFValOf[DFBits[8]]]
//  x := 1
//  val bitsToken = DFBits(8) token b0s
//  val bitsConst = DFBits(8) const bitsToken
//  val barry = new Bar
//  foo((1, 2))
//  (DFBits(8), DFBit) <> IN init ?

object Bla extends App {
  val top = new Foo
  top.printCodeString()
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
