import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._


trait Bar
trait Foo {
  object Baz{
    type TBar <: Bar
  }
  val a : Baz.TBar
}

object Fooy extends Foo {
  object Baz {
    trait TBar extends Bar {
      def printer : Unit = {}
    }
  }
  val a  = new Baz.TBar {}
}

Fooy.a.printer

