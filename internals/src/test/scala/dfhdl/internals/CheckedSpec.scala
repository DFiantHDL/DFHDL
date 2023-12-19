package dfhdl.internals

import munit.*

import scala.compiletime.testing.typeCheckErrors

object Positive
    extends Check1[
      Int,
      [t <: Int] =>> t > 0,
      [t <: Int] =>> "Must be positive, but found: " + t
    ]

def positive[T <: Int](arg: T)(using check: Positive.Check[arg.type]): Unit =
  check(arg)

object LHSBiggerThanRHS
    extends Check2[
      Int,
      Int,
      [lhs <: Int, rhs <: Int] =>> lhs > rhs,
      [lhs <: Int, rhs <: Int] =>> "The LHS argument (" + lhs +
        ") is NOT bigger than the RHS argument (" + rhs + ")."
    ]
extension [L <: Int](lhs: L)
  infix def biggerThan[R <: Int](rhs: R)(using
      check: LHSBiggerThanRHS.Check[lhs.type, rhs.type]
  ): Unit =
    check(lhs, rhs)

class CheckedSpec extends FunSuite:
  positive(5)
  val badPositive = typeCheckErrors(
    """positive(-1)"""
  ).head.message
  assertEquals(badPositive, "Must be positive, but found: -1")

  val five = 5
  val negOne = -1
  positive(five)
  interceptMessage[java.lang.IllegalArgumentException](
    "Must be positive, but found: -1"
  ) {
    positive(negOne)
  }

  5 biggerThan 3
  val badBiggerThan = typeCheckErrors(
    """3 biggerThan 5"""
  ).head.message
  assertEquals(
    badBiggerThan,
    "The LHS argument (3) is NOT bigger than the RHS argument (5)."
  )

  val three = 3
  5 biggerThan three
  five biggerThan three
  five biggerThan 3
  interceptMessage[java.lang.IllegalArgumentException](
    "The LHS argument (3) is NOT bigger than the RHS argument (5)."
  ) {
    three biggerThan five
  }
  interceptMessage[java.lang.IllegalArgumentException](
    "The LHS argument (3) is NOT bigger than the RHS argument (5)."
  ) {
    3 biggerThan five
  }
  interceptMessage[java.lang.IllegalArgumentException](
    "The LHS argument (3) is NOT bigger than the RHS argument (5)."
  ) {
    three biggerThan 5
  }
end CheckedSpec
