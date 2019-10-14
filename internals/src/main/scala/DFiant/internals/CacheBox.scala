package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable

import CacheBoxRO.Boxed
sealed class CacheBoxRO[+T](updateFunc : => T) {
  private val deps : mutable.Set[CacheBoxRO[_]] = mutable.Set()
  protected[this] var boxed : Boxed[T] = Boxed.Empty
  @inline final protected def getBoxed : Boxed[T] = boxed
  @inline final protected def boxIsEmpty : Boolean = boxed match {
    case Boxed.Empty => true
    case Boxed.Visited(_) => true
    case _ => false
  }
  @inline final protected def boxVisit() : Unit = boxed match {
    case Boxed.Empty => boxed = Boxed.Visited(1)
    case Boxed.Visited(n) => boxed = Boxed.Visited(n + 1)
    case _ => //Do not change
  }
  @inline final protected def boxCyclic() : Unit = boxed = Boxed.CyclicError
  @inline final protected def boxClear() : Unit = boxed = Boxed.Empty
  @inline final protected[internals] def boxUpdate() : Unit = {
    boxed = Boxed.ValidValue(updateFunc)
  }
  @inline protected def emptyBoxUpdate() : Unit = if (boxIsEmpty) boxUpdate()
  @tailrec private def dirty(current : CacheBoxRO[_], remainingDeps : List[CacheBoxRO[_]]) : Unit = {
    val updatedDeps = current.getBoxed match {
      case Boxed.Empty => remainingDeps
      case _ =>
        current.boxClear()
        remainingDeps ++ current.deps
    }
    updatedDeps match {
      case Nil =>
      case x :: xs => dirty(x, xs)
    }
  }
  @tailrec private def updateSrcBoxes(curDeps : List[CacheBoxRO[_]], curSrcs : List[CacheBoxRO[_]]) : Unit = curSrcs match {
    case Nil => curDeps.foreach(x => x.boxUpdate())
    case x :: xs =>
      val (updatedDeps, updatedSrcs) = x.getBoxed match {
        case Boxed.Empty =>
          //adding in reverse to updated empty values in topological dependency order
          x match {
            case sd : CacheDerivedRO[_] => (x +: curDeps, xs ++ sd.sources)
            case sd : CacheDerivedROList[_] => (x +: curDeps, xs ++ sd.sources)
            case _ => (x +: curDeps, xs)
          }
        case _ => (curDeps, xs)
      }
      x.boxVisit()
      updateSrcBoxes(updatedDeps, updatedSrcs)
  }
  @inline final protected def updateSrcBoxes() : Unit = updateSrcBoxes(List(), List(this))
  @inline final def unbox : T = {
    emptyBoxUpdate()
    boxed match {
      case b : Boxed.Value[T] => b.value
      case b => throw new IllegalArgumentException(b.toString)
    }
  }
  @inline final override def toString: String = unbox.toString
  @inline final protected def dirty() : Unit = dirty(this, List())
  @inline final protected def dirtyDeps() : Unit = deps.foreach{d => d.dirty()}
  @inline final def addDependency(st : CacheBoxRO[_]) : Unit = deps += st
  @inline final def removeDependency(st : CacheBoxRO[_]) : Unit = deps -= st
}
object CacheBoxRO {
  sealed abstract class Boxed[+T] extends Product with Serializable
  object Boxed {
    sealed abstract class Value[+T] extends Boxed[T] {val value : T}
    case class ValidValue[+T](value : T) extends Value[T]
    case class CircularValue[+T](value : T) extends Value[T]
    case object Empty extends Boxed[Nothing]
    case object CyclicError extends Boxed[Nothing]
    case class Visited(num : Int) extends Boxed[Nothing]
  }

  @inline def apply[T](updateFunc : => T) : CacheBoxRO[T] = new CacheBoxRO[T](updateFunc)
  @inline implicit def toValue[T](sf : CacheBoxRO[T]) : T = sf.unbox
}

sealed class CacheBoxRW[T](default : T) extends CacheBoxRO[T](default) {
  @inline def set(newValue : T) : Unit = {
    boxed = Boxed.ValidValue(newValue)
    dirtyDeps()
  }
}
object CacheBoxRW {
  def apply[T](t : T) : CacheBoxRW[T] = new CacheBoxRW[T](t)
}

final case class CacheListRW[T](default : List[T]) extends CacheBoxRW[List[T]](default) {
  private val deps : mutable.ListBuffer[CacheDerivedHashMapRO[_,_,_]] = mutable.ListBuffer()
  @inline def += (deltaValue : T) : Unit = {
    super.set(unbox :+ deltaValue)
    pushAddUpdates()
  }
  @inline def setDefault() : Unit = {
    super.set(default)
    pushSetDefault()
  }
  @inline override def set(newValue : List[T]) : Unit = {
    setDefault()
    newValue.foreach(x => this += x)
  }
  @inline private def pushAddUpdates() : Unit = deps.foreach(x => x.add())
  @inline private def pushSetDefault() : Unit = deps.foreach(x => x.setDefault())

  @inline protected[internals] def addFolderDependency(st : CacheDerivedHashMapRO[_,_,_]) : Unit = deps += st
}

final case class CacheDerivedHashMapRO[A, B, T]
  (source : CacheListRW[T])(default : Map[A, B])
  (op : (Map[A, B], T) => Map[A, B]) extends CacheBoxRO(default) {
  @inline protected[internals] def add() : Unit =  {
    boxed = Boxed.ValidValue(op(unbox, source.unbox.last))
    dirtyDeps()
  }
  @inline protected[internals] def setDefault() : Unit = {
    boxed = Boxed.ValidValue(default)
    dirtyDeps()
  }
  source.addFolderDependency(this)
}
//object CacheDerivedFolderRO {
//  def apply[T, TR <: immutable.Iterable[T], ST, STR[ST0] <: immutable.Iterable[ST0], SR]
//  (source: CacheFolderRO[ST, STR[ST], SR])(default: TR)(op: (TR, ST) => TR)
//  : CacheDerivedFolderRO[T, TR, ST, STR[ST], SR] = new CacheDerivedFolderRO[T, TR, ST, STR[ST], SR](source)(default)(op)
//}

//sealed class CacheListDerivedRO[+T](default : List[T])(source : CacheListRO[T]) extends CacheListRO[T](default)



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

final class CacheDerivedRO[+T](val sources : List[CacheBoxRO[_]])(updateFunc : => T) extends CacheBoxRO[T](updateFunc) {
  sources.foreach{s => s.addDependency(this)}
}

final class CacheDerivedROList[+T](stBoxList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T) extends CacheBoxRO[T](updateFunc) {
  var stList : Option[List[CacheBoxRO[_]]] = None
  def sources : List[CacheBoxRO[_]] = stBoxList.unbox
  @inline override def emptyBoxUpdate() : Unit = if (boxIsEmpty) {
    val updateList = Some(stBoxList.unbox)
    if (updateList != stList) {
      stList.foreach{t => t.foreach {x => x.removeDependency(this)}}
      updateList.foreach{t => t.foreach {x => x.addDependency(this)}}
      dirty()
    }
    updateSrcBoxes()
  }
  stBoxList.addDependency(this)
}

object CacheDerivedRO {
  def apply[T](cb : CacheBoxRO[_]*)(updateFunc : => T) : CacheBoxRO[T] = new CacheDerivedRO(cb.toList)(updateFunc)
  def apply[T](cb : List[CacheBoxRO[_]])(updateFunc : => T) : CacheBoxRO[T] = new CacheDerivedRO(cb)(updateFunc)
  def apply[T](cbList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T) : CacheBoxRO[T] = new CacheDerivedROList(cbList)(updateFunc)
}

