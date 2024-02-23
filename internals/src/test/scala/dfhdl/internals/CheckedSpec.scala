package dfhdl.internals

import munit.*

import scala.compiletime.testing.typeCheckErrors

object Positive
    extends Check1[
      Int,
      [t <: Int] =>> t > 0,
      [t <: Int] =>> "Must be positive, but found: " + t
    ]
object LHSBiggerThanRHS
    extends Check2[
      Int,
      Int,
      [lhs <: Int, rhs <: Int] =>> lhs > rhs,
      [lhs <: Int, rhs <: Int] =>> "The LHS argument (" + lhs +
        ") is NOT bigger than the RHS argument (" + rhs + ")."
    ]

class CheckedSpec extends FunSuite:
  def positive[T <: Int](arg: T)(using check: Positive.Check[arg.type]): Unit =
    check(arg)

  extension [L <: Int](lhs: L)
    infix def biggerThan[R <: Int](rhs: R)(using
        check: LHSBiggerThanRHS.Check[lhs.type, rhs.type]
    ): Unit =
      check(lhs, rhs)

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

class CheckedNUBSpec extends FunSuite:
  def positive[T](arg: T)(using check: Positive.CheckNUB[arg.type]): Unit =
    arg match
      case arg: Int => check(arg)
      case _        => throw new java.lang.IllegalArgumentException("Not an int.")

  extension [L](lhs: L)
    infix def biggerThan[R](rhs: R)(using
        check: LHSBiggerThanRHS.CheckNUB[lhs.type, rhs.type]
    ): Unit =
      (lhs, rhs) match
        case (lhs: Int, rhs: Int) => check(lhs, rhs)
        case _                    => throw new java.lang.IllegalArgumentException("Not an int.")

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
  interceptMessage[java.lang.IllegalArgumentException](
    "Not an int."
  ) {
    positive("negOne")
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
  interceptMessage[java.lang.IllegalArgumentException](
    "Not an int."
  ) {
    three biggerThan "5"
  }
end CheckedNUBSpec
