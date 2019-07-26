package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable

sealed class StateBoxRO[+T](updateFunc : => T) {
  private val deps : mutable.HashSet[StateBoxRO[_]] = mutable.HashSet()
  protected[this] var value : Option[T] = None
  @inline final protected def valueIsEmpty : Boolean = value.isEmpty
  @inline final protected def valueClear() : Unit = value = None
  @inline final protected def valueUpdate() : Unit = value = Some(updateFunc)
  @inline protected def emptyValueUpdate() : Unit = if (value.isEmpty) updateSrcValues()
  @tailrec private def dirty(current : StateBoxRO[_], remainingDeps : List[StateBoxRO[_]]) : Unit = {
    val updatedDeps = if (!current.valueIsEmpty) {
      current.valueClear()
      remainingDeps ++ current.deps
    } else remainingDeps
    updatedDeps match {
      case Nil =>
      case x :: xs => dirty(x, xs)
    }
  }
  @tailrec final protected def updateSrcValues(curDeps : List[StateBoxRO[_]], curSrcs : List[StateBoxRO[_]]) : Unit = curSrcs match {
    case Nil => curDeps.foreach(x => x.valueUpdate())
    case x :: xs =>
      if (x.valueIsEmpty) {
        val updatedDeps = x +: curDeps //adding in reverse to updated empty values in topological dependency order
        x match {
          case sd : StateDerivedRO[_] => updateSrcValues(updatedDeps, xs ++ sd.sources)
          case sd : StateDerivedROList[_] => updateSrcValues(updatedDeps, xs ++ sd.sources)
          case _ => updateSrcValues(updatedDeps, xs)
        }
      } else updateSrcValues(curDeps, xs)
  }
  @inline final protected def updateSrcValues() : Unit = updateSrcValues(List(), List(this))
  @inline final def get : T = {
    emptyValueUpdate()
    value.get
  }
  @inline final override def toString: String = get.toString
  @inline final protected def dirty() : Unit = dirty(this, List())
  @inline final protected def dirtyDeps() : Unit = deps.foreach{d => d.dirty()}
  @inline final def addDependency(st : StateBoxRO[_]) : Unit = deps += st
  @inline final def removeDependency(st : StateBoxRO[_]) : Unit = deps -= st
}
object StateBoxRO {
  @inline def apply[T](updateFunc : => T) : StateBoxRO[T] = new StateBoxRO[T](updateFunc)
  @inline implicit def toValue[T](sf : StateBoxRO[T]) : T = sf.get
}

final class StateBoxRW[T](default : T) extends StateBoxRO[T](default) {
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

final class StateDerivedRO[+T](val sources : List[StateBoxRO[_]])(updateFunc : => T) extends StateBoxRO[T](updateFunc) {
  sources.foreach{s => s.addDependency(this)}
}

final class StateDerivedROList[+T](stBoxList : StateBoxRO[List[StateBoxRO[_]]])(updateFunc : => T) extends StateBoxRO[T](updateFunc) {
  var stList : Option[List[StateBoxRO[_]]] = None
  def sources : List[StateBoxRO[_]] = stBoxList.get
  @inline override def emptyValueUpdate() : Unit = if (value.isEmpty) {
    val updateList = Some(stBoxList.get)
    if (updateList != stList) {
      stList.foreach{t => t.foreach {x => x.removeDependency(this)}}
      updateList.foreach{t => t.foreach {x => x.addDependency(this)}}
      dirty()
    }
    updateSrcValues()
  }
  stBoxList.addDependency(this)
}

object StateDerivedRO {
  def apply[T](st : StateBoxRO[_]*)(updateFunc : => T) : StateBoxRO[T] = new StateDerivedRO(st.toList)(updateFunc)
  def list[T](stBoxList : StateBoxRO[List[StateBoxRO[_]]])(updateFunc : => T) : StateBoxRO[T] = new StateDerivedROList(stBoxList)(updateFunc)
}

