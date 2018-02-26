trait Foo[T] {
  val value : T
}
object Foo {
  implicit def from[T <: Int](t : T)(implicit bld : Builder[T]) : Foo[bld.Out] = bld(t)
}

trait Builder[T]{
  type Out
  def apply(t : T) : Foo[Out]
}
object Builder {
  type Aux[T, Out0] = Builder[T]{type Out = Out0}
  implicit def evInt : Aux[Int, Int] = new Builder[Int] {
    type Out = Int
    def apply(t : Int) : Foo[Out] = new Foo[Int]{val value = t}
  }
}

//import Foo._
val a : Foo[Int] = 1
