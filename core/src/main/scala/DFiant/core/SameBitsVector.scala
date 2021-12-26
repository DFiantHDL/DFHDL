package DFiant
package core

/** A unconstrained-width literal vector of a given bit value
  *
  * @example
  * {{{
  *   val x = DFBits(8) init all(0)
  *   x := all(1)
  * }}}
  *
  * @note
  *   Some vector operations are not possible with this literal. E.g., `x ++
  *   all(0)` is forbidden because concatenation cannot infer the output width
  *   from this operation.
  */
final case class SameBitsVector(value: Bit) derives CanEqual

object SameBitsVector:
  given [W <: Int]: CanEqual[SameBitsVector, DFBits[W] <> VAL] =
    CanEqual.derived
