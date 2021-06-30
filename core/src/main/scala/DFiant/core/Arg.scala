package DFiant.core
import DFiant.internals.*

object Arg:
  object Width
    extends Check1[
      Int,
      [t <: Int] =>> t > 0,
      [t <: Int] =>> "Width must be positive, but found: " + t
    ]