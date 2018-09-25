package DFiant.internals

import DFiant.internals.LazyBox.ValueOrError

//final class LazyBox[+T](value : => T) {
//  private var visited : Boolean = false
//  lazy val valueOption : Option[T] = {
//    if (visited) None
//    else {
//      visited = true
//      Some(value)
//    }
//  }
//  @inline def getOrElse[B >: T](default: => B): B = valueOption.getOrElse(default)
//}
//object LazyBox {
//  def apply[T](value : => T) : LazyBox[T] = new LazyBox[T](value)
//}

//(args : List[MutableLazyBox[_]], func : List[MutableLazyBox[_]] => T)
abstract class LazyBox[+T] private (path : String) {
  import LazyBox.ValueOrError._
  def valueFunc : ValueOrError[T]
  private var visited : Boolean = false
  private var locked : Boolean = false
  private[this] var valueOrError : ValueOrError[T] = Error(path, "Uninitialized")
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
  def get : T = getValueOrError match {
    case Value(v) => v
    case Error(p, m) => throw new IllegalArgumentException(s"\n$m at $p")
  }
}


object LazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(path : String, msg : String) extends ValueOrError[Nothing]
  }
  case class Mutable[T](path : String)(initialization : Option[T] = None) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    private var mutableValueFunc : () => ValueOrError[T] = () => initialization match {
      case Some(t) => Value(t)
      case _ => Error(path, "Uninitialized")
    }
    final def valueFunc : ValueOrError[T] = mutableValueFunc()
    private var isset = false
    final def isSet : Boolean = isset
    def set(value : LazyBox[T]) : Unit = {
      mutableValueFunc = () => value.getValueOrError
      isset = true
    }
  }
  case class Const[+T](path : String)(value : T) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = Value(value)
  }
  case class Args1C[+T, +A, C](path : String)(func : (A, C) => T, arg : LazyBox[A], const : C) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(s"$p <- $path", m)
      case Value(a) => Value(func(a, const))
    }
  }
  case class Args2[+T, +L, +R](path : String)(func : (L, R) => T, leftArg : LazyBox[L], rightArg : LazyBox[R]) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (leftArg.getValueOrError, rightArg.getValueOrError) match {
      case (Error(p, m), _) => Error(s"$p <- $path", m)
      case (_, Error(p, m)) => Error(s"$p <- $path", m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }
  case class Args3[+T, +A1, +A2, +A3](path : String)(func : (A1, A2, A3) => T, arg1 : LazyBox[A1], arg2 : LazyBox[A2], arg3 : LazyBox[A3]) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (arg1.getValueOrError, arg2.getValueOrError, arg3.getValueOrError) match {
      case (Error(p, m), _, _) => Error(s"$p <- $path", m)
      case (_, Error(p, m), _) => Error(s"$p <- $path", m)
      case (_, _, Error(p, m)) => Error(s"$p <- $path", m)
      case (Value(a1), Value(a2), Value(a3)) => Value(func(a1, a2, a3))
    }
  }
  case class ArgList[+T, +L](path : String)(func : List[L] => T, argList : List[LazyBox[L]]) extends LazyBox[T](path){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = {
      val xs = argList.map(a => a.getValueOrError)
      xs collectFirst {case x : Error => x} getOrElse Value(func(xs.collect {case Value(x) => x}))
    }
  }
}

