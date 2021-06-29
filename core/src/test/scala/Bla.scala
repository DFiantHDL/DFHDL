import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val x = DFBit <> IN init 1
  val bitsToken = DFBits(8) token b0s
  val bitsConst = DFBits(8) const bitsToken
  val barry = new Bar

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
