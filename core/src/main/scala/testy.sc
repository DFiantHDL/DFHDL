
object Test {
  class Foo(val i : Int)
  trait Bar {
  }
  object Bar {
    implicit def ev(foo : Foo) : Foo with Bar = foo.asInstanceOf[Foo with Bar]
  }
}

import Test._
val a : Foo with Bar = new Foo(5)