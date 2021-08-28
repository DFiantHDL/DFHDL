package DFiant

extension [T](t: T)
  def verifyTypeOf[R](using T <:< R): T = t
  def verifyTokenOf[R <: DFType](using T <:< core.DFToken.Of[R]): T = t
