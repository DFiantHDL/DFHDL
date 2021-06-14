package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFDecimal[S <: Boolean, W <: Int, F <: Int] <: DFType.Of[
  ir.DFDecimal
] = DFType.Of[ir.DFDecimal]
object DFDecimal:
  def apply[S <: Boolean, W <: Int, F <: Int](
      signed: Inlined.Boolean[S],
      width: Inlined.Int[W],
      fractionWidth: Inlined.Int[F]
  ): DFDecimal[S, W, F] =
    ir.DFDecimal(signed, width, fractionWidth).asInstanceOf[DFDecimal[S, W, F]]
  opaque type Token[S <: Boolean, W <: Int, F <: Int] <: DFToken.Of[
    DFDecimal[S, W, F],
    Option[BigInt]
  ] = DFToken.Of[DFDecimal[S, W, F], Option[BigInt]]

type DFUInt[W <: Int] = DFDecimal[false, W, 0]
object DFUInt:
  def apply[W <: Int](width: Inlined.Int[W]): DFUInt[W] =
    DFDecimal(false, width, 0)
  type Token[W <: Int] = DFDecimal.Token[false, W, 0]

type DFSInt[W <: Int] = DFDecimal[true, W, 0]
object DFSInt:
  def apply[W <: Int](width: Inlined.Int[W]): DFSInt[W] =
    DFDecimal(true, width, 0)
  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
