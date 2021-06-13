package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFDecimal[S <: Boolean, W <: Int, F <: Int] <: ir.DFDecimal =
  ir.DFDecimal
object DFDecimal:
  def apply[S <: Boolean, W <: Int, F <: Int](
      signed: Inlined.Boolean[S],
      width: Inlined.Int[W],
      fractionWidth: Inlined.Int[F]
  ): DFDecimal[S, W, F] =
    ir.DFDecimal(signed, width, fractionWidth)
