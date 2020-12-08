package DFiant.internals

class CloneClassWithContextSpec extends munit.FunSuite {
  trait Ctx { self =>
    val value: Int = 1
    def updateValue(v: Int): Ctx =
      new Ctx {
        override val value: Int = v
        override def cloneWithContext(arg: Ctx): Any =
          self.cloneWithContext(arg)
      }
    def cloneWithContext(arg: Ctx): Any
  }
  object Ctx {
    implicit def ev[T](implicit c: CloneClassWithContext[Ctx]): Ctx =
      arg => c(arg)
  }
  trait Cloneable {
    val ctx: Ctx
    def cloneWithContext(arg: Ctx): this.type =
      ctx.cloneWithContext(arg).asInstanceOf[this.type]
  }
  class TestClass(val i: Int)(implicit val ctx: Ctx) extends Cloneable
  class TestClass3(val i: Int)(val ii: Int)(val iii: Int)(implicit val ctx: Ctx)
      extends Cloneable
  abstract class TestAbstractClass(implicit val ctx: Ctx) extends Cloneable
  trait TestTrait extends TestAbstractClass {
    val x = 5
  }
  case class TestCaseClass(i: Int)(val ii: Int)(implicit val ctx: Ctx)
      extends Cloneable

  trait Foo
  implicit object Foo extends Foo
  class TestClassFoo(val i: Int)(implicit val ctx: Ctx, foo: Foo)
      extends Cloneable

  test("Clone of class with single argument") {
    val x          = new TestClass(1)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of anonymous class with single argument") {
    val x          = new TestClass(1) {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of class with single argument and other implicits") {
    val x          = new TestClassFoo(1)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of anonymous class with single argument and other implicits") {
    val x          = new TestClassFoo(1) {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of abstract class") {
    val x          = new TestAbstractClass {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of trait") {
    val x          = new TestTrait {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.ctx.value == 2)
    assert(cloneX.x == 5)
  }

  test("Clone of class with multiple arguments") {
    val x          = new TestClass3(1)(2)(3)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.iii == 3)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of anonymous class with multiple arguments") {
    val x          = new TestClass3(1)(2)(3) {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.iii == 3)
    assert(cloneX.ctx.value == 2)
  }

  test("Clone of case class with multiple arguments") {
    val x          = TestCaseClass(1)(2)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX     = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.ctx.value == 2)
  }
}
