package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFOpaque[N <: String, T <: DFType] <: ir.DFOpaque = ir.DFOpaque
object DFOpaque:
  def apply[N <: String, T <: DFType](name: N, actualType: T): DFOpaque[N, T] =
    ir.DFOpaque(name, actualType)
