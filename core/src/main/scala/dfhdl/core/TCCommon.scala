package dfhdl.core
import dfhdl.internals.Exact1
trait TCCommon[T <: DFTypeAny, V, O]
    extends Exact1.TC[DFTypeAny, T, [t <: DFTypeAny] =>> t, V, DFC]:
  type Out <: O
