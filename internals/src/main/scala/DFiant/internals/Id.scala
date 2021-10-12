package DFiant.internals

trait Id[T]:
  type Out = T
given [T]: Id[T] with {}
