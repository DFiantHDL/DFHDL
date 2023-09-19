package dfhdl.core

trait TCConv[T <: DFTypeAny, V, O]:
  type Out <: O
  type Ctx
  def conv(dfType: T, value: V)(using Ctx): Out
