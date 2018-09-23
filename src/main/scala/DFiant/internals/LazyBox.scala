package DFiant.internals

import DFiant.internals.MutableLazyBox.ValueOrError

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
  import MutableLazyBox.ValueOrError._
  def valueFunc : ValueOrError[T]
  private var visited : Boolean = false
  private var locked : Boolean = false
  private var valueOrError : ValueOrError[T] = Error(path, "Uninitialized")
  def getValueOrError : ValueOrError[T] = {
    if (visited) Error(path, "Circular dependency detected")
    else {
      if (!locked) {
        visited = true
        valueOrError = valueFunc
        visited = false
        locked = true
      }
      valueOrError
    }
  }
  private def clearValue() : Unit = {
    valueOrError = Error(path,"Uninitialized")
    locked = false
  }
//  @inline def getOrElse[B >: T](default: => B): B
//  {
//    valueOption.getOrElse(default)
//  }
}


object MutableLazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(path : String, msg : String) extends ValueOrError[Nothing]
  }
  case class Const[T](path : String)(value : T) extends MutableLazyBox[T](path){
    import MutableLazyBox.ValueOrError._
    final val valueFunc = Value(value)
  }
  case class Args2[T, L, R](path : String)(leftArg : MutableLazyBox[L], rightArg : MutableLazyBox[R], func : (L, R) => T) extends MutableLazyBox[T](path){
    import MutableLazyBox.ValueOrError._
    final val valueFunc = (leftArg.valueOrError, rightArg.valueOrError) match {
      case (Error(p, m), _) => Error(s"$p <- $path", m)
      case (_, Error(p, m)) => Error(s"$p <- $path", m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }

//  def apply[T](value : => T) : MutableLazyBox[T] = new MutableLazyBox[T](value)
}

