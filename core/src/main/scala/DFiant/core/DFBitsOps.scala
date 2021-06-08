package DFiant
package core
import internals.*
import scala.annotation.targetName

extension [LW <: Int](lhs: DFBits.Token[LW])
  @targetName("concat")
  def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
    val width = lhs._width + rhs._width
    val valueBits = lhs.valueBits ++ rhs.valueBits
    val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
    DFBits.Token(width)(valueBits, bubbleBits)
