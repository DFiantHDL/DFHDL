import DFiant._
import scodec.bits._
import GlobalDesign._

trait FooGeneric
abstract class Foo(implicit f : this.type => Unit) extends FooGeneric {
  f(this)
}

object Bar extends Foo




