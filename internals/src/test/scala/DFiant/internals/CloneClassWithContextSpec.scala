package DFiant.internals

import org.scalatest.flatspec.AnyFlatSpec

class CloneClassWithContextSpec extends AnyFlatSpec {
  trait Ctx {self =>
    val value : Int = 1
    def updateValue(v : Int) : Ctx = new Ctx {
      override val value : Int = v
      override def cloneWithContext(arg : Ctx) : Any = self.cloneWithContext(arg)
    }
    def cloneWithContext(arg : Ctx) : Any
  }
  object Ctx {
    implicit def ev[T](implicit c : CloneClassWithContext[Ctx]) : Ctx = arg => c(arg)
  }
  trait Cloneable {
    val ctx : Ctx
    def cloneWithContext(arg : Ctx) : this.type = ctx.cloneWithContext(arg).asInstanceOf[this.type]
  }
  class TestClass(val i : Int)(implicit val ctx : Ctx) extends Cloneable
  class TestClass3(val i : Int)(val ii : Int)(val iii : Int)(implicit val ctx : Ctx) extends Cloneable
  abstract class TestAbstractClass(implicit val ctx : Ctx) extends Cloneable
  trait TestTrait extends TestAbstractClass {
    val x = 5
  }
  case class TestCaseClass(i : Int)(val ii : Int)(implicit val ctx : Ctx) extends Cloneable

  "Clone of class with single argument" should "work" in {
    val x = new TestClass(1)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  "Clone of anonymous class with single argument" should "work" in {
    val x = new TestClass(1){}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ctx.value == 2)
  }

  "Clone of abstract class" should "work" in {
    val x = new TestAbstractClass{}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.ctx.value == 2)
  }

  "Clone of trait" should "work" in {
    val x = new TestTrait {}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.ctx.value == 2)
    assert(cloneX.x == 5)
  }

  "Clone of class with multiple arguments" should "work" in {
    val x = new TestClass3(1)(2)(3)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.iii == 3)
    assert(cloneX.ctx.value == 2)
  }

  "Clone of anonymous class with multiple arguments" should "work" in {
    val x = new TestClass3(1)(2)(3){}
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.iii == 3)
    assert(cloneX.ctx.value == 2)
  }

  "Clone of case class with multiple arguments" should "work" in {
    val x = TestCaseClass(1)(2)
    val updatedCtx = x.ctx.updateValue(2)
    val cloneX = x.cloneWithContext(updatedCtx)
    assert(cloneX.i == 1)
    assert(cloneX.ii == 2)
    assert(cloneX.ctx.value == 2)
  }
}
