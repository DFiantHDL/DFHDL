trait Unsafe
object Unsafe extends Unsafe

trait Binary {
  val value : Int
}
object Binary {
  def apply(constVal : 0) : Int = 0
  def apply(constVal : 1)(implicit dummy : DummyImplicit) : Int = 1
  def apply(constVal : Int)(implicit unsafe: Unsafe) : Int = constVal match {
    case 0 => 0
    case 1 => 1
  }
}

object SafeTest {
  val zero = Binary(0)
  val one = Binary(1)
  val failing = Binary(2) //fails at compile-time
}

object UnsafeTest {
  implicit val unsafe = Unsafe //must be
  for (i <- 0 to 2)
    Binary(i) //fails at run-time
}
