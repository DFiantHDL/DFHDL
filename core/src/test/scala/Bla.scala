import DFiant.*

class Foo(using DFC) extends DFDesign:
  val x = DFBits(8) <> IN
  val z = DFBits(8) <> OUT
  val y = (DFBits(8), (DFBit, DFBool)) <> OUT
//  y := (x, (1, 1))
  z := (x, x)
  z := Tuple1(x)

object Bla extends App:
  val top = new Foo
  top.printCodeString()
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
