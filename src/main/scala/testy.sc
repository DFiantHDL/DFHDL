import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._


trait Foo {
  trait Bar
  val a : Bar = ???
}

object Fooy extends Foo {
  trait Bar extends super.Bar {
    def printer : Unit = {}
  }
}

Fooy.a.printer

