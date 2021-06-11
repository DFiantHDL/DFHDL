package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFOpaque[T <: DFType] <: ir.DFOpaque = ir.DFOpaque
object DFOpaque:
  def apply[T <: DFType](actualType: T)(using
      meta: MetaContext
  ): DFOpaque[T] = ir.DFOpaque(meta.name, actualType)
