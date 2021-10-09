package DFiant
package core

enum SameBitsVector(value: Boolean) derives CanEqual:
  /** A unconstrained-width literal vector of zeros
    *
    * @example
    *   {{{val x = DFBits(8) init; b0s x := b0s}}}
    *
    * @note
    *   Some vector operations are not possible with this literal. E.g., `x ++
    *   b0s` is forbidden because concatenation cannot infer the output width
    *   from this operation.
    */
  case b0s extends SameBitsVector(false)

  /** A unconstrained-width literal vector of ones
    *
    * @example
    *   {{{val x = DFBits(8) init b1s; x := b1s}}}
    *
    * @note
    *   Some vector operations are not possible with this literal. E.g., `x ++
    *   b1s` is forbidden because concatenation cannot infer the output width
    *   from this operation.
    */
  case b1s extends SameBitsVector(true)
