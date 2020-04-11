trait Context {
  type Out
}
object Context {
  type Aux[Out0] = Context {type Out = Out0}
  implicit def ev : Aux[1] = new Context {
    println("shit")
    type Out = 1
  }
}
class Foo(implicit ctx : Context)

class Bar(implicit ctx : Context) {
  new Foo
  new Foo
}

val b = new Bar

