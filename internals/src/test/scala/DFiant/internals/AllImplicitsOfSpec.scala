package DFiant.internals

class AllImplicitsOfSpec extends munit.FunSuite {
  trait Foo
  implicit object Foo  extends Foo
  implicit object Foo2 extends Foo

  test("find all implicits of Foo") {
    implicitly[AllImplicitsOf[Foo]].list == List(Foo, Foo2)
  }

  trait NoImpl
  test("find no implicits of NoImpl") {
    implicitly[AllImplicitsOf[NoImpl]].list == List()
  }
}
