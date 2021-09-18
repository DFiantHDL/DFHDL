package DFiant
import internals.Inlined

extension [T](t: T)
  def verifyTypeOf[R](using T <:< R): T = t
  def verifyTokenOf[R <: DFType](using T <:< core.DFToken.Of[R]): T = t

extension [T](t: Inlined[T])
  def verifyInlined[R](r: Inlined[R])(using T =:= R): Inlined[T] =
    assert(t.value == r.value)
    t
