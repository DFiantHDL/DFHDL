package DFiant.internals

extension [T](t: T)
  def debugPrint: T =
    println(t)
    t
