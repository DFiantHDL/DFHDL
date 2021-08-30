package DFiant.core
import DFiant.internals.*

import scala.compiletime.ops.int.ToString

object Arg:
  object Width
      extends Check1[
        Int,
        [t <: Int] =>> t > 0,
        [t <: Int] =>> "Width must be positive, but found: " + ToString[t]
      ]
  object SignedWidth
      extends Check1[
        Int,
        [t <: Int] =>> t > 1,
        [t <: Int] =>> "Signed value width must be larger than 1, but found: " + ToString[
          t
        ]
      ]
end Arg
