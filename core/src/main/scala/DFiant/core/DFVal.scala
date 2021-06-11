package DFiant
package core
import internals.*

sealed trait DFModifier
object DFModifier

trait DFVal[+T <: DFType, +M <: DFModifier]
