import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val x = DFUInt(8) <> IN init (1, 2, 3)
  val c = DFBit const 1
  val bb = b"11111111"
  val bbv: DFBits.Token[8] = bb
  val barry = new Bar

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
  summon[core.DFToken.TC[DFBits[8], DFBits.Token[8]]]
}
