package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFOpaque[T <: DFType, ID] <: ir.DFOpaque = ir.DFOpaque
object DFOpaque:
  def apply[T <: DFType, ID](actualType: T)(using
      meta: MetaContext
  ): DFOpaque[T, ID] = ir.DFOpaque(meta.name, actualType)
