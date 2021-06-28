import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val x = DFUInt(8) <> IN init (1, 2, 3)
  val bitConst = DFBit token 1
  val bb: DFBits.Token[7] = b"1111111"
//  val bitsConst = DFBits(8) token bb
  val barry = new Bar

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
