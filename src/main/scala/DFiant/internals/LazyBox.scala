package DFiant.internals

final class LazyBox[+T](value : => T) {
  private var visited : Boolean = false
  lazy val valueOption : Option[T] = {
    if (visited) None
    else {
      visited = true
      Some(value)
    }
  }
  @inline def getOrElse[B >: T](default: => B): B = valueOption.getOrElse(default)
}
object LazyBox {
  def apply[T](value : => T) : LazyBox[T] = new LazyBox[T](value)
}

