package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

//TODO: move back with companions once https://github.com/lampepfl/dotty/issues/13461
//is resolved
opaque type DFDecimal[S <: Boolean, W <: Int, F <: Int] <: DFType.Of[
  ir.DFDecimal
] = DFType.Of[ir.DFDecimal]
