import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign:
  println(typeName)
class Foo(using DFC) extends DFDesign:
  val bool = true
//  val x = DFBit <> IN init (bool, true, 0, 1)
  val y = (DFBits(8), DFBit) <> IN init ((h"11", 1))
//  val bitsToken = DFBits(8) token b0s
//  val bitsConst = DFBits(8) const bitsToken
//  val barry = new Bar
//  foo((1, 2))
//  (DFBits(8), DFBit) <> IN init ?

object Bla extends App {
  val top = new Foo
  val db = top.getDB
  println(top.getDB)
//  summon[core.DFToken.TC[DFBits[6], DFBits.Token[8]]]
}
