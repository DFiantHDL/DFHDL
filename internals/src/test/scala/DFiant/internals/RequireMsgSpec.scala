package dfhdl.internals
import munit.*
import scala.compiletime.testing.typeCheckErrors
class InlinedRequireSpec extends FunSuite:
  Inlined.require(true, "xyz")
  val falseCond = false

  interceptMessage[java.lang.IllegalArgumentException]("xyz") {
    Inlined.require(falseCond, "xyz")
  }

  val summonedFalse = typeCheckErrors(
    """Inlined.require(false, "xyz")"""
  ).head.message
  assertEquals(summonedFalse, "xyz")

  inline def positive[T <: Int](inline value: Int): Unit =
    Inlined.require(value > 0, s"expected positive but found: $value")

  val negTwo = -2
  positive(5)
  interceptMessage[java.lang.IllegalArgumentException](
    "expected positive but found: -2"
  ) {
    positive(negTwo)
  }

  val positiveError = typeCheckErrors(
    """positive(-7)"""
  ).head.message
  assertEquals(positiveError, "expected positive but found: -7")
end InlinedRequireSpec
