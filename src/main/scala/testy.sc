import DFiant._

import shapeless._
case class Foo() extends DFFields {
  val u = DFUInt(8)
  val b = DFBits(8)
}

val fooGen = LabelledGeneric[Foo]

val a = Foo()


fooGen.to(a)
val b = fooGen.from(HNil)