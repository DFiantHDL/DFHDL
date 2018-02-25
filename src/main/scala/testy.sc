trait Gen
trait FooPrinter[F <: Gen] {
  def apply(f : F) : Unit
}
abstract class Foo[F <: Gen](implicit printer : FooPrinter[F]) extends Gen {
  printer(this.asInstanceOf[F])
}

trait Bar[Value] extends Foo[Bar[Value]] {val value : Value}
implicit val barInt : FooPrinter[Bar[Int]] = foo => {}

new Bar[Int] {
  val value : Int = 0
}