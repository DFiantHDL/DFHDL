package DFiant.internals

import munit.*

import scala.compiletime.testing.typeCheckErrors

object RequirePositive
    extends Check1[
      Int,
      [t <: Int] =>> t > 0,
      [t <: Int] =>> "Must be positive, but found: " + t
    ]

class CheckedSpec extends FunSuite:
  val x = summon[RequirePositive.Check[55]]
  x(55)
