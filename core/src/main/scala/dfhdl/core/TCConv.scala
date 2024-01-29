package dfhdl.core

trait TCConv[T <: DFTypeAny, V, O]:
  type Out <: O
  def conv(dfType: T, value: V)(using DFC): Out
