import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val x = DFUInt(8) <> IN init (1, 2, 3)
  val c = DFBits(8) const b0s
  val barry = new Bar

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
  summon[core.DFToken.TC[DFBits[8], DFBits.Token[2]]]
}
