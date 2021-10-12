package DFiant.internals

trait Id[T]:
  type Out = T
object Id:
  given [T]: Id[T] = new Id[T] {}
