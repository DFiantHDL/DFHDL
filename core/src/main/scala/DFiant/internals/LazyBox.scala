package DFiant.internals
import LazyBox.ValueOrError
import scala.collection.mutable

abstract class LazyBox[+T] private (args : List[LazyBox[_]], fallBackValue : Option[T] = None)(implicit n : NameIt) {
  import LazyBox.ValueOrError._
  import LazyBox.CustomException
  def valueFunc : ValueOrError[T]
  val owner : DSLMemberConstruct
  final val name : String = n.value
  final lazy val fullName = s"${owner.fullName}.$name"
  lazy val typeName: String = s"LazyBox.${getClass.getSimpleName}"
  private var visitedCnt : Int = 0
  private var locked : Boolean = false
  private[this] var valueOrError : ValueOrError[T] = Error(List(this), "Uninitialized")
  private def getUpdatedValueOrError : ValueOrError[T] =
    try {
      valueFunc
    } catch  {
      case e : CustomException =>
        Error(this :: e.lbTrace, e.msg)
      case e : Exception =>
        Error(List(this), s"Exception occured when calculating LazyBox value: ${e.getMessage}")
    }
  private val valueDependencies : mutable.Set[LazyBox[_]] = mutable.Set.empty[LazyBox[_]]
  final def getDependencyNum : Int = valueDependencies.size
  final protected def unlockValueDependencies() : Unit = if (locked) {
    locked = false
    valueDependencies.foreach(vd => vd.unlockValueDependencies())
  }
  final protected def addValueDependency(lb : LazyBox[_]) : Unit = {
//    if (owner != lb.owner) println(f"$fullName%-40s <- ${lb.fullName}")
    valueDependencies += lb
  }
  final protected def clearValueDependencies() : Unit = valueDependencies.clear()
  private def getCircularError : ValueOrError[T] = Error(List(this), "Circular dependency detected")
  protected def getFallBackValue : ValueOrError[T] = fallBackValue match {
    case Some(fb) => Value(fb)
    case None => getUpdatedValueOrError
  }
  var checkFallBack : Boolean = false
  final def getValueOrError : ValueOrError[T] = {
    if (!locked) {
      visitedCnt += 1
      valueOrError = visitedCnt match {
        case 1 =>
          val updatedValueOrError = getUpdatedValueOrError
          (updatedValueOrError, valueOrError) match {
            case (Value(vNew), Value(vOld)) if vNew != vOld && checkFallBack =>
              checkFallBack = false
              Error(List(this), s"Contradiction in circular dependency $vNew != $vOld")
            case _ => updatedValueOrError
          }
        case 2 =>
          checkFallBack = true
          getFallBackValue
        case _ => getCircularError
      }
//      if (visitedCnt == 1 && name == "pipeLB" && !owner.asInstanceOf[DFiant.DFAny].isAnonymous)
//        println(f"$this%-60s $valueOrError")
//      assert(false)

      visitedCnt -= 1
      locked = true
    }
    valueOrError
  }
  def clearValue() : Unit = {
//    println(f"$this%-60s CLEAR!")
    valueOrError = Error(List(this),"Uninitialized")
    unlockValueDependencies()
  }
  final def get : T = getValueOrError match {
    case Value(v) => v
    case Error(p, m) => throw new CustomException(p, m)
  }
  args.foreach(a => a.addValueDependency(this))

  override def toString: String = s"${owner.fullName}.$name"
}


object LazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(lbTrace : List[LazyBox[_]], msg : String) extends ValueOrError[Nothing]
  }
  class CustomException(val lbTrace : List[LazyBox[_]], val msg : String) extends Exception(msg) {
    override def getMessage: String = {
      val pStr = lbTrace.map(lb => s"${lb.owner.fullName}.${lb.name}").mkString(" <- ")
      s"\n$msg at $pStr"
    }
  }

  //cdFallBack - in case of circular dependency, fallback to the initialization value
  case class Mutable[T](owner : DSLMemberConstruct)(initialization : => T)(implicit n : NameIt) extends LazyBox[T](List(), Some(initialization)){
    import LazyBox.ValueOrError._
    private var lbox : LazyBox[T] = LazyBox.Const(owner)(initialization)
    def getBox : LazyBox[T] = lbox
    final def valueFunc : ValueOrError[T] = lbox.getValueOrError
    private var isset = false
    final def isSet : Boolean = isset
    def set(value : LazyBox[T]) : Unit = {
//      clearValueDependencies()
      clearValue()
      addValueDependency(value)
      unlockValueDependencies()
      lbox = value
      isset = true
    }
    def set(value : => T) : Unit = set(Const(owner)(value))
  }
  case class Const[+T](owner : DSLMemberConstruct)(value : => T)(implicit n : NameIt) extends LazyBox[T](List()){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = Value(value)
  }
  case class Args1[+T, +A](owner : DSLMemberConstruct)(func : A => T, arg : LazyBox[A], fallBackValue : Option[T] = None)(implicit n : NameIt) extends LazyBox[T](List(arg), fallBackValue){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(a) => Value(func(a))
    }
  }
  case class Args1C[+T, +A, C](owner : DSLMemberConstruct)(func : (A, C) => T, arg : LazyBox[A], const : => C)(implicit n : NameIt) extends LazyBox[T](List(arg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(a) => Value(func(a, const))
    }
  }
  case class Args2[+T, +L, +R](owner : DSLMemberConstruct)(func : (L, R) => T, leftArg : LazyBox[L], rightArg : LazyBox[R])(implicit n : NameIt) extends LazyBox[T](List(leftArg, rightArg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (leftArg.getValueOrError, rightArg.getValueOrError) match {
      case (Error(p, m), _) => Error(this :: p, m)
      case (_, Error(p, m)) => Error(this :: p, m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }
  case class Args3[+T, +A1, +A2, +A3](owner : DSLMemberConstruct)(func : (A1, A2, A3) => T, arg1 : LazyBox[A1], arg2 : LazyBox[A2], arg3 : LazyBox[A3])(implicit n : NameIt) extends LazyBox[T](List(arg1, arg2, arg3)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (arg1.getValueOrError, arg2.getValueOrError, arg3.getValueOrError) match {
      case (Error(p, m), _, _) => Error(this :: p, m)
      case (_, Error(p, m), _) => Error(this :: p, m)
      case (_, _, Error(p, m)) => Error(this :: p, m)
      case (Value(a1), Value(a2), Value(a3)) => Value(func(a1, a2, a3))
    }
  }
  case class Args1List[+T, +A, +L](owner : DSLMemberConstruct)(func : (A, List[L]) => T, arg : LazyBox[A], argList : List[LazyBox[L]])(implicit n : NameIt) extends LazyBox[T](arg :: argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(argVal) =>
        val xs = argList.map(a => a.getValueOrError)
        xs collectFirst {case x : Error => x} getOrElse Value(func(argVal, xs.collect {case Value(x) => x}))
    }
  }
  case class ArgList[+T, +L](owner : DSLMemberConstruct)(func : List[L] => T, argList : List[LazyBox[L]])(implicit n : NameIt) extends LazyBox[T](argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = {
      val xs = argList.map(a => a.getValueOrError)
      xs collectFirst {case x : Error => x} getOrElse Value(func(xs.collect {case Value(x) => x}))
    }
  }
}

