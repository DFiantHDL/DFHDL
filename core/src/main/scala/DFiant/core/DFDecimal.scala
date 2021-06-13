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

type DFUInt[W <: Int] = DFDecimal[false, W, 0]
object DFUInt

type DFSInt[W <: Int] = DFDecimal[true, W, 0]
object DFSInt
