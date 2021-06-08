package DFiant
package core
import internals.*
import scala.annotation.targetName

object DFBitsOps:
  extension [LW <: Int](lhs: DFBits.Token[LW])
    @targetName("concat")
    def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
      val width = lhs.width + rhs.width
      val valueBits = lhs.valueBits ++ rhs.valueBits
      val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
      DFBits.Token(width)(valueBits, bubbleBits)

// @targetName("bitwiseAnd")
// def &[RW <: Int](rhs: DFBits.Token[RW])(using
//     bb: Bubble.Behaviour
// ): DFBits.Token[LW] =
//   assert(lhs.width == rhs.width)
//   bb match
//     case Bubble.Stall =>
//       Token(left.valueBits & right.valueBits, left.bubbleMask | right.bubbleMask)
//     case Bubble.DontCare =>
//       val valueBits = (left.valueBits | left.bubbleMask) & (right.valueBits | right.bubbleMask)
//       val bubbleMask = (left.bubbleMask & right.bubbleMask) | (left.bubbleMask & right.valueBits) |
//         (right.bubbleMask & left.valueBits)
//       Token(valueBits, bubbleMask)

//   val width = lhs.width
//   val valueBits = lhs.valueBits ++ rhs.valueBits
//   val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
//   DFBits.Token(width)(valueBits, bubbleBits)
