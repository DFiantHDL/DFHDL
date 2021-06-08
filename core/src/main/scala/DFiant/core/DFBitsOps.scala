package DFiant
package core
import internals.*
import scala.annotation.targetName

val ont = 1
type MMM = ont.type
extension [LW <: Int](lhs: DFBits.Token[LW])
  @targetName("concat")
  def ++[RW <: Int](rhs: DFBits.Token[MMM]): DFBits.Token[LW + RW] =
    val dfType = DFBits(lhs._width + rhs._width)
    ???
//DFToken(dfType)(???)
