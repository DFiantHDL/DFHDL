package DFiant.core
import DFiant.internals.*

object Arg:
  object Width
      extends Check1[
        Int,
        [t <: Int] =>> t > 0,
        [t <: Int] =>> "Width must be positive, but found: " + t
      ]
  object SignedWidth
      extends Check1[
        Int,
        [t <: Int] =>> t > 1,
        [t <: Int] =>> "Signed value width must be larger than 1, but found: " + t
      ]
  object Positive
      extends Check1[
        Int,
        [t <: Int] =>> t > 0,
        [t <: Int] =>> "Argument must be positive, but found: " + t
      ]
  object LargerThan1
      extends Check1[
        Int,
        [t <: Int] =>> t > 1,
        [t <: Int] =>> "Argument must be larger than 1, but found: " + t
      ]
end Arg
