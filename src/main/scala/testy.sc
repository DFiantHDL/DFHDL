//val s1 = "oron.port.is.the.best"
//val s2 = "oron.port.is.te"
//def comp(callFullName : String, refFullName : String) : String = {
//  val c = callFullName.split('.')
//  val r = refFullName.split('.')
//  if (r.length < c.length)
//    return refFullName
//
//  val res = for {i <- (math.max(c.length, r.length)-1) to 0 by -1; if c.length <= i || (c(i) != r(i))} yield r(i)
//
//  res.mkString(".")
//
//
//}
//comp(s2,s1)

trait Context {
  val owner : Option[Foo]
}
trait ContextOf[T] extends Context
trait LowPriorityContext {
  implicit def fallback[T] : ContextOf[T] = new ContextOf[T] {
    val owner: Option[Foo] = None
  }
}
object ContextOf extends LowPriorityContext {
  implicit def ev[T](implicit evOwner : Foo) : ContextOf[T] =
    new ContextOf[T]{val owner = Some(evOwner)}
}

abstract class Foo(implicit ctx : Context) {
  val owner : Option[Foo] = ctx.owner
  implicit val childOwner : Foo = this
}

abstract class A(implicit ctx : ContextOf[A]) extends Foo
abstract class AA(implicit ctx : ContextOf[AA]) extends Foo {
  val a = new A {}
}


val a = new AA{}
a.owner == None
a.a.owner.get == a