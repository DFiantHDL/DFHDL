import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val x = DFBits(8) <> IN init h"11"
  val bitConst = DFBit token 1
  val bb = h"11"
  val bitsConst = DFBits(8) token bb
  val barry = new Bar

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
