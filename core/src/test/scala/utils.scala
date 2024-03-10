package dfhdl
import internals.Inlined

extension [T](t: T)
  def verifyTypeOf[R](using T <:< R): T = t
  def verifyValOf[R <: DFType](using T <:< core.DFValOf[R]): T = t
