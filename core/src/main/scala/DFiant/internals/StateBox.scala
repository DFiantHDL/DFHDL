package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable

class StateBoxRO[+T](updateFunc : => T) {
  private val deps : mutable.HashSet[StateBoxRO[_]] = mutable.HashSet()
  protected[this] var value : Option[T] = None
  @inline final protected def valueSnoop : Option[T] = value
  @tailrec private def dirty(current : StateBoxRO[_], remainingDeps : List[StateBoxRO[_]]) : Unit = {
    val updatedDeps = if (current.valueSnoop.isDefined) {
      value = None
      remainingDeps ++ current.deps
    } else remainingDeps
    updatedDeps match {
      case Nil =>
      case x :: xs => dirty(x, xs)
    }
  }

  @inline def get : T = value.getOrElse {
    val updateValue = updateFunc
    value = Some(updateValue)
    updateValue
  }
  @inline final override def toString: String = get.toString
  @inline final def dirty() : Unit = dirty(this, List())
  final protected def dirtyDeps() : Unit = deps.foreach{d => d.dirty()}
  final def addDependency(st : StateBoxRO[_]) : Unit = deps += st
  final def removeDependency(st : StateBoxRO[_]) : Unit = deps -= st
}
object StateBoxRO {
  def apply[T](updateFunc : => T) : StateBoxRO[T] = new StateBoxRO[T](updateFunc)
  implicit def toValue[T](sf : StateBoxRO[T]) : T = sf.get
}

class StateBoxRW[T](default : T) extends StateBoxRO[T](default) {
  @inline def set(newValue : T) : Unit = {
    value = Some(newValue)
    dirtyDeps()
  }
}
object StateBoxRW {
  def apply[T](t : T) : StateBoxRW[T] = new StateBoxRW[T](t)
}

//case class StateDerivedRW[T, R](st : StateBoxRW[T])(t2r : T => R)(r2t : (T, R) => T) extends StateBoxRW[R](t2r(st)) {
//  private var oldT : T = st
//  @inline override def set(newValue : R) : Unit = {
//    val oldR = super.get
//    if (newValue != oldR) {
//      super.set(newValue)
//      val newT = r2t(st.get, newValue)
//      st.set(newT)
//      oldT = newT
//    }
//  }
//  @inline override def get : R = {
//    val newT = st.get
//    if (oldT == newT) super.get
//    else {
//      oldT = newT
//      val newR = t2r(newT)
//      super.set(newR)
//      newR
//    }
//  }
//}

class StateDerivedRO[+T](stList : List[StateBoxRO[_]])(updateFunc : => T) extends StateBoxRO[T](updateFunc) {
  stList.foreach{s => s.addDependency(this)}
}

class StateDerivedROList[+T](stBoxList : StateBoxRO[List[StateBoxRO[_]]])(updateFunc : => T) extends StateBoxRO[T](updateFunc) {
  var stList : Option[List[StateBoxRO[_]]] = None
  @inline override def get : T = value.getOrElse {
    val updateList = Some(stBoxList.get)
    if (updateList != stList) {
      stList.foreach{t => t.foreach {x => x.removeDependency(this)}}
      updateList.foreach{t => t.foreach {x => x.addDependency(this)}}
      dirty()
    }
    super.get
  }
  stBoxList.addDependency(this)
}

object StateDerivedRO {
  def apply[T](st : StateBoxRO[_]*)(updateFunc : => T) : StateBoxRO[T] = new StateDerivedRO(st.toList)(updateFunc)
  def list[T](stBoxList : StateBoxRO[List[StateBoxRO[_]]])(updateFunc : => T) : StateBoxRO[T] = new StateDerivedROList(stBoxList)(updateFunc)
}

