package dfhdl
import internals.Inlined

extension [T](t: T)
  def verifyTypeOf[R](using T <:< R): T = t
  def verifyValOf[R <: DFType](using T <:< core.DFValOf[R]): T = t

extension [T](t: Inlined[T])
  def verifyInlined[R](
      r: Inlined[R]
  )(using T =:= R, CanEqual[T, R]): Inlined[T] =
    assert(t.value == r.value)
    t
