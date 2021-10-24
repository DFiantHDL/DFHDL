package DFiant.core

trait GeneralTC[T <: DFTypeAny, -V, O]:
  type Out <: O
  def apply(dfType: T, value: V): Out
