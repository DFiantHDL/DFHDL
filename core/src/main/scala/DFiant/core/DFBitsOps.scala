package DFiant
package core
import internals.*
import scala.annotation.targetName

extension [LW <: Int](lhs: DFBits.Token[LW])
  @targetName("concat")
  def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
    val dfType = DFBits(lhs._width + rhs._width)
    DFToken(dfType)(???)
