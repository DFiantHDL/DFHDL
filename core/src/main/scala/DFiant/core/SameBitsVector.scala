package DFiant
package core

enum SameBitsVector(val value: Bit) derives CanEqual:
  /** A unconstrained-width literal vector of zeros
    *
    * @example
    *   {{{val x = DFBits(8) init; Zeros x := Zeros}}}
    *
    * @note
    *   Some vector operations are not possible with this literal. E.g., `x ++
    *   Zeros` is forbidden because concatenation cannot infer the output width
    *   from this operation.
    */
  case Zeros extends SameBitsVector(0)

  /** A unconstrained-width literal vector of ones
    *
    * @example
    *   {{{val x = DFBits(8) init Ones; x := Ones}}}
    *
    * @note
    *   Some vector operations are not possible with this literal. E.g., `x ++
    *   Ones` is forbidden because concatenation cannot infer the output width
    *   from this operation.
    */
  case Ones extends SameBitsVector(1)
end SameBitsVector

object SameBitsVector:
  given [W <: Int]: CanEqual[SameBitsVector, DFBits[W] <> VAL] =
    CanEqual.derived
