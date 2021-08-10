package DFiant.core

trait GeneralTC[T <: DFType, -V, O]:
  type Out <: O
  def apply(dfType: T, value: V): Out
