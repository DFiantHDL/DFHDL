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

//(args : List[MutableLazyBox[_]], func : List[MutableLazyBox[_]] => T)
abstract class MutableLazyBox[T] private (path : String) {
  var value : () => T
  private var visited : Boolean = false
  private var locked : Boolean = false
  private var valueOption : Either[T, String] = Right("Uninitialized")
  protected def updateValue() : Unit
  private def clearValue() : Unit = {
    valueOption = Right("Uninitialized")
    locked = false
  }
  @inline def getOrElse[B >: T](default: => B): B
//  {
//      if (visited) None
//      else {
//        visited = true
//        Some(value())
//      }
//    valueOption.getOrElse(default)
//  }
}


object MutableLazyBox {

//  def apply[T](value : => T) : MutableLazyBox[T] = new MutableLazyBox[T](value)
}

