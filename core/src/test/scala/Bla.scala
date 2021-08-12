import DFiant.*

class Foo(using DFC) extends DFDesign:
  val x = DFBits(8) <> IN
  val y = (DFBits(8), (DFBit, DFBool)) <> OUT
  y := (h"11", (1, 1))

object Bla extends App:
  val top = new Foo
  top.printCodeString()
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
