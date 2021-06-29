import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val bool = true
  val x = DFBit <> IN init bool
  val bitsToken = DFBits(8) token b0s
  val bitsConst = DFBits(8) const bitsToken
  val barry = new Bar
  val y = (DFBit, DFBits(8)) token (true, h"11")
object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
