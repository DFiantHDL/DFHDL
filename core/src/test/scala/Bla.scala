import DFiant.*
import internals.*
class Bar(using DFC) extends DFDesign

class Foo(using DFC) extends DFDesign:
  val barry = new Bar

object Bla extends App {
  val top = new Foo
}
