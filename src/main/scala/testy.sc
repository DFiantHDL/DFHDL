import DFiant._

import shapeless._
case class Foo() {
  implicit val ed = new DFDesign() {}
  val u = DFUInt(8)
  val b = DFBits(8)
}


//Foo2.copy
val fooGen = LabelledGeneric[Foo]

//val a = Foo()


//fooGen.to(a)
//val b = fooGen.from(HNil)