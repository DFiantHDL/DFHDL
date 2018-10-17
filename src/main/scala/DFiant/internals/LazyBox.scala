package DFiant.internals

import DFiant.internals.LazyBox.ValueOrError

import scala.collection.mutable

abstract class LazyBox[+T] private (owner : DSLMemberConstruct)(args : List[LazyBox[_]]) {
  import LazyBox.ValueOrError._
  def valueFunc : ValueOrError[T]
  private var visited : Boolean = false
  private var locked : Boolean = false
  private[this] var valueOrError : ValueOrError[T] = Error(List(owner), "Uninitialized")
  private val valueDependencies : mutable.Set[LazyBox[_]] = mutable.Set.empty[LazyBox[_]]
  final protected def unlockValueDependencies() : Unit = if (locked) {
    locked = false
    valueDependencies.foreach(vd => vd.unlockValueDependencies())
  }
  final protected def addValueDependency(lb : LazyBox[_]) : Unit = valueDependencies += lb
  final def getValueOrError : ValueOrError[T] = {
    if (visited) Error(List(owner), "Circular dependency detected")
    else {
      if (!locked) {
        visited = true
        valueOrError = try {
          valueFunc
        } catch  {
          case e : Exception =>
            Error(List(owner), s"Exception occured when calculating LazyBox value: ${e.getMessage}")
        }
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
  final def get : T = getValueOrError match {
    case Value(v) => v
    case Error(p, m) => {
      val pStr = p.map(o => o.fullName).mkString(" <- ")
      throw new IllegalArgumentException(s"\n$m at $pStr")
    }
  }
  args.foreach(a => a.addValueDependency(this))
}


object LazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(path : List[DSLMemberConstruct], msg : String) extends ValueOrError[Nothing]
  }
  case class Mutable[T](owner : DSLMemberConstruct)(initialization : Option[T] = None) extends LazyBox[T](owner)(List()){
    import LazyBox.ValueOrError._
    private var mutableValueFunc : () => ValueOrError[T] = () => initialization match {
      case Some(t) => Value(t)
      case _ => Error(List(owner), "Uninitialized")
    }
    final def valueFunc : ValueOrError[T] = mutableValueFunc() match {
      case Error(p, m) => Error(owner :: p, m)
      case Value(v) => Value(v)
    }
    private var isset = false
    final def isSet : Boolean = isset
    def set(value : LazyBox[T]) : Unit = {
      addValueDependency(value)
      unlockValueDependencies()
      mutableValueFunc = () => value.getValueOrError
      isset = true
    }
    def set(value : T) : Unit = set(Const(owner)(value))
  }
  case class Const[+T](owner : DSLMemberConstruct)(value : T) extends LazyBox[T](owner)(List()){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = Value(value)
  }
  case class Args1C[+T, +A, C](owner : DSLMemberConstruct)(func : (A, C) => T, arg : LazyBox[A], const : C) extends LazyBox[T](owner)(List(arg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(owner :: p, m)
      case Value(a) => Value(func(a, const))
    }
  }
  case class Args2[+T, +L, +R](owner : DSLMemberConstruct)(func : (L, R) => T, leftArg : LazyBox[L], rightArg : LazyBox[R]) extends LazyBox[T](owner)(List(leftArg, rightArg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (leftArg.getValueOrError, rightArg.getValueOrError) match {
      case (Error(p, m), _) => Error(owner :: p, m)
      case (_, Error(p, m)) => Error(owner :: p, m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }
  case class Args3[+T, +A1, +A2, +A3](owner : DSLMemberConstruct)(func : (A1, A2, A3) => T, arg1 : LazyBox[A1], arg2 : LazyBox[A2], arg3 : LazyBox[A3]) extends LazyBox[T](owner)(List(arg1, arg2, arg3)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (arg1.getValueOrError, arg2.getValueOrError, arg3.getValueOrError) match {
      case (Error(p, m), _, _) => Error(owner :: p, m)
      case (_, Error(p, m), _) => Error(owner :: p, m)
      case (_, _, Error(p, m)) => Error(owner :: p, m)
      case (Value(a1), Value(a2), Value(a3)) => Value(func(a1, a2, a3))
    }
  }
  case class Args1List[+T, +A, +L](owner : DSLMemberConstruct)(func : (A, List[L]) => T, arg : LazyBox[A], argList : List[LazyBox[L]]) extends LazyBox[T](owner)(arg :: argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(owner :: p, m)
      case Value(argVal) =>
        val xs = argList.map(a => a.getValueOrError)
        xs collectFirst {case x : Error => x} getOrElse Value(func(argVal, xs.collect {case Value(x) => x}))
    }
  }
  case class ArgList[+T, +L](owner : DSLMemberConstruct)(func : List[L] => T, argList : List[LazyBox[L]]) extends LazyBox[T](owner)(argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = {
      val xs = argList.map(a => a.getValueOrError)
      xs collectFirst {case x : Error => x} getOrElse Value(func(xs.collect {case Value(x) => x}))
    }
  }
}

