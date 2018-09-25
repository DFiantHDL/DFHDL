package DFiant.internals

import DFiant.internals.LazyBox.ValueOrError

abstract class LazyBox[+T] private (owner : DSLMemberConstruct) {
  import LazyBox.ValueOrError._
  def valueFunc : ValueOrError[T]
  private var visited : Boolean = false
  private var locked : Boolean = false
  private[this] var valueOrError : ValueOrError[T] = Error(List(owner), "Uninitialized")
  def getValueOrError : ValueOrError[T] = {
    if (visited) Error(List(owner), "Circular dependency detected")
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
    valueOrError = Error(List(owner),"Uninitialized")
    locked = false
  }
  def get : T = getValueOrError match {
    case Value(v) => v
    case Error(p, m) => {
      val pStr = p.map(o => o.fullName).mkString(" <- ")
      throw new IllegalArgumentException(s"\n$m at $pStr")
    }
  }
}


object LazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(path : List[DSLMemberConstruct], msg : String) extends ValueOrError[Nothing]
  }
  case class Mutable[T](owner : DSLMemberConstruct)(initialization : Option[T] = None) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    private var mutableValueFunc : () => ValueOrError[T] = () => initialization match {
      case Some(t) => Value(t)
      case _ => Error(List(owner), "Uninitialized")
    }
    final def valueFunc : ValueOrError[T] = mutableValueFunc()
    private var isset = false
    final def isSet : Boolean = isset
    def set(value : LazyBox[T]) : Unit = {
      mutableValueFunc = () => value.getValueOrError
      isset = true
    }
  }
  case class Const[+T](owner : DSLMemberConstruct)(value : T) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = Value(value)
  }
  case class Args1C[+T, +A, C](owner : DSLMemberConstruct)(func : (A, C) => T, arg : LazyBox[A], const : C) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(p :+ owner, m)
      case Value(a) => Value(func(a, const))
    }
  }
  case class Args2[+T, +L, +R](owner : DSLMemberConstruct)(func : (L, R) => T, leftArg : LazyBox[L], rightArg : LazyBox[R]) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (leftArg.getValueOrError, rightArg.getValueOrError) match {
      case (Error(p, m), _) => Error(p :+ owner, m)
      case (_, Error(p, m)) => Error(p :+ owner, m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }
  case class Args3[+T, +A1, +A2, +A3](owner : DSLMemberConstruct)(func : (A1, A2, A3) => T, arg1 : LazyBox[A1], arg2 : LazyBox[A2], arg3 : LazyBox[A3]) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (arg1.getValueOrError, arg2.getValueOrError, arg3.getValueOrError) match {
      case (Error(p, m), _, _) => Error(p :+ owner, m)
      case (_, Error(p, m), _) => Error(p :+ owner, m)
      case (_, _, Error(p, m)) => Error(p :+ owner, m)
      case (Value(a1), Value(a2), Value(a3)) => Value(func(a1, a2, a3))
    }
  }
  case class ArgList[+T, +L](owner : DSLMemberConstruct)(func : List[L] => T, argList : List[LazyBox[L]]) extends LazyBox[T](owner){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = {
      val xs = argList.map(a => a.getValueOrError)
      xs collectFirst {case x : Error => x} getOrElse Value(func(xs.collect {case Value(x) => x}))
    }
  }
}

