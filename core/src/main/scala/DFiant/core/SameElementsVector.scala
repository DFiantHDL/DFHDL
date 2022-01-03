package DFiant
package core

import internals.*
import scala.annotation.targetName

/** A unconstrained-width literal vector of a given bit value
  *
  * @example
  * {{{
  *   val x = DFBits(8) init all(0)
  *   x := all(1)
  * }}}
  * @note
  *   Some vector operations are not possible with this literal. E.g., `x ++
  *   all(0)` is forbidden because concatenation cannot infer the output width
  *   from this operation.
  */
final class SameElementsVector[T](val value: T) derives CanEqual

object SameElementsVector:
  def apply[T](exact: Inlined[T]): SameElementsVector[T] =
    new SameElementsVector[T](exact)
  // hacked unapply will be replaced by plugin
  def unapply[T, R](arg: SameElementsVector[T]): Option[R] = Some(
    arg.value.asInstanceOf[R]
  )
  given eqBit[W <: Int, T <: BitOrBool]
      : CanEqual[SameElementsVector[T], DFBits[W] <> VAL] =
    CanEqual.derived
  given eqVec[DFT <: DFTypeAny, D <: NonEmptyTuple, T]
      : CanEqual[SameElementsVector[T], DFVector[DFT, D] <> VAL] =
    CanEqual.derived
end SameElementsVector
