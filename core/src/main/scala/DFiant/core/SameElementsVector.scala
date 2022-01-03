package DFiant
package core

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
final case class SameElementsVector[T](value: T) derives CanEqual

object SameElementsVector:
  def unapply[T <: Bit](arg: SameElementsVector[T]): Option[Bit] = Some(
    arg.value
  )
//  @targetName("unapplyBoolean")
//  def unapply[T <: Boolean](arg: SameElementsVector[T]): Option[Boolean] = Some(
//    arg.value
//  )
  given eqBit[W <: Int, T <: BitOrBool]
      : CanEqual[SameElementsVector[T], DFBits[W] <> VAL] =
    CanEqual.derived
  given eqVec[DFT <: DFTypeAny, D <: NonEmptyTuple, T]
      : CanEqual[SameElementsVector[T], DFVector[DFT, D] <> VAL] =
    CanEqual.derived
end SameElementsVector
