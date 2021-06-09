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

    @targetName("bitwiseAnd")
    def &[RW <: Int](rhs: DFBits.Token[RW])(using
        bb: Bubble.Behaviour
    ): DFBits.Token[LW] =
      assert(lhs.width == rhs.width)
      val width = lhs.width
      bb match
        case Bubble.Behaviour.Stall =>
          DFBits.Token(width)(
            lhs.valueBits & rhs.valueBits,
            lhs.bubbleBits | rhs.bubbleBits
          )
        case Bubble.Behaviour.DontCare =>
          val valueBits =
            (lhs.valueBits | lhs.bubbleBits) & (rhs.valueBits | rhs.bubbleBits)
          val bubbleBits =
            (lhs.bubbleBits & rhs.bubbleBits) | (lhs.bubbleBits & rhs.valueBits) |
              (rhs.bubbleBits & lhs.valueBits)
          DFBits.Token(width)(valueBits, bubbleBits)
