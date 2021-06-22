package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFOpaque[N <: String, T <: DFType] <: DFType.Of[ir.DFOpaque] =
  DFType.Of[ir.DFOpaque]
object DFOpaque:
  def apply[N <: String, T <: DFType](name: N, actualType: T): DFOpaque[N, T] =
    ir.DFOpaque(name, actualType.asIR).asInstanceOf[DFOpaque[N, T]]

  object Ops:
    extension [T <: DFType.Supported](t: T)(using tc: DFType.TC[T])
      def opaque(using
          name: CTName
      ): DFOpaque[name.Out, tc.Type] =
        DFOpaque[name.Out, tc.Type](name.value, tc(t))
