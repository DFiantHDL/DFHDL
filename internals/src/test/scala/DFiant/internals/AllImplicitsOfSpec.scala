package DFiant.internals

import org.scalatest.flatspec.AnyFlatSpec

class AllImplicitsOfSpec extends AnyFlatSpec {
  trait Foo
  implicit object Foo extends Foo
  implicit object Foo2 extends Foo

  "AllImplicitsOf" should "find all implicits of Foo" in {
    implicitly[AllImplicitsOf[Foo]].list == List(Foo, Foo2)
  }

  trait NoImpl
  "AllImplicitsOf" should "find no implicits of NoImpl" in {
    implicitly[AllImplicitsOf[NoImpl]].list == List()
  }
}
