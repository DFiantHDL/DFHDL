package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

sealed trait DFModifier
object DFModifier

opaque type DFVal[+T <: DFType, +M <: DFModifier] = ir.DFVal
