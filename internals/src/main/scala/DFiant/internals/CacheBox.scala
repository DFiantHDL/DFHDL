package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable

object CacheBox {
  case class Owner(cb : Nameable)
}

import CacheBoxRO.Boxed
sealed class CacheBoxRO[+T](updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) {
  lazy val fullName : String = s"${owner.cb.__dev.nameScala}.${name.value}"
  //Dependencies
  private val deps : mutable.Set[CacheBoxRO[_]] = mutable.Set()
  @inline final def addDependency(st : CacheBoxRO[_]) : Unit = deps += st
  @inline final def removeDependency(st : CacheBoxRO[_]) : Unit = deps -= st
  //Sources
  @inline protected def sources : List[CacheBoxRO[_]] = List()
  //Boxed value or flags
  private[this] var _boxed : Boxed[T] = Boxed.Empty
  @inline final protected def boxed : Boxed[T] = _boxed
  @inline final protected[this] def boxed_=(that : Boxed[T]) : Unit = _boxed = that

  @inline final protected def boxIsEmpty : Boolean = boxed match {
    case Boxed.Empty => true
    case Boxed.Visited => true
    case _ => false
  }
  @inline final protected def boxIsCyclicError : Boolean = boxed match {
    case Boxed.CyclicError => true
    case _ => false
  }

  @inline final protected def boxVisit() : Unit = boxed match {
    case Boxed.Empty => boxed = Boxed.Visited
    case _ => //Do not change
  }
  @inline final protected def boxClear() : Unit = boxed = Boxed.Empty
  protected def boxUpdate() : Unit = {
    val isCyclic = sources.map(s => s.boxed).map {
      case Boxed.Empty  => true
      case Boxed.Visited => true
      case Boxed.CyclicError => true
      case _ => false
    }.foldLeft(false)((l, r) => l || r)
    if (isCyclic) boxed = Boxed.CyclicError
    else boxed = Boxed.ValidValue(updateFunc)
  }
  @tailrec private def dirty(current : CacheBoxRO[_], remainingDeps : List[CacheBoxRO[_]]) : Unit = {
    val updatedDeps = current.boxed match {
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
  @tailrec private def update(curDeps : List[CacheBoxRO[_]], curSrcs : List[CacheBoxRO[_]]) : Unit = curSrcs match {
    case Nil => curDeps.foreach(x => x.boxUpdate())
    case x :: xs =>
      //adding in reverse to updated empty values in topological dependency order
      val (updatedDeps, updatedSrcs) = if (x.boxIsEmpty) (x +: curDeps, x.sources ++ xs) else (curDeps, xs)
      x.boxVisit()
      update(updatedDeps, updatedSrcs)
  }
  @inline protected def update() : Unit = boxed match {
    case Boxed.Visited => boxed = Boxed.CyclicError
    case Boxed.Empty => update(List(), List(this))
    case _ => //Nothing to do
  }
  @inline final protected def updateSrcs() : Unit = sources.foreach(s => s.update())
  final def getBoxed : Boxed[T] = {
    update()
    boxed
  }
  @inline final def unbox : T = {
    update()
    boxed match {
      case b : Boxed.Value[T] => b.value
      case b =>
        throw new IllegalArgumentException(s"$fullName:$b")
    }
  }
  @inline final override def toString: String = unbox.toString
  @inline final protected def dirty() : Unit = dirty(this, List())
  @inline final protected def dirtyDeps() : Unit = deps.foreach{d => d.dirty()}
}
object CacheBoxRO {
  sealed abstract class Boxed[+T] extends Product with Serializable
  object Boxed {
    sealed abstract class Value[+T] extends Boxed[T] {val value : T}
    sealed abstract class NoValue extends Boxed[Nothing]
    case class ValidValue[+T](value : T) extends Value[T]
    case object Visited extends NoValue
    case object Empty extends NoValue
    case object CyclicError extends NoValue
  }

  @inline def apply[T](updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) : CacheBoxRO[T] = new CacheBoxRO[T](updateFunc)
  @inline implicit def toValue[T](sf : CacheBoxRO[T]) : T = sf.unbox
}

sealed class CacheBoxRW[T](default : T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) extends CacheBoxRO[T](default) {
  @inline def set(newValue : T) : Unit = {
    boxed = Boxed.ValidValue(newValue)
    dirtyDeps()
  }
}
object CacheBoxRW {
  def apply[T](t : T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) : CacheBoxRW[T] = new CacheBoxRW[T](t)
}

final case class CacheListRW[T](default : List[T])(implicit owner : CacheBox.Owner, name : sourcecode.Name) extends CacheBoxRW[List[T]](default) {
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
  (op : (Map[A, B], T) => Map[A, B])(implicit owner : CacheBox.Owner, name : sourcecode.Name) extends CacheBoxRO(default) {
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

final class CacheDerivedRO[+T](override val sources : List[CacheBoxRO[_]])(updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) extends CacheBoxRO[T](updateFunc) {
  sources.foreach{s => s.addDependency(this)}
}

final class CacheDerivedROList[+T](stBoxList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) extends CacheBoxRO[T](updateFunc) {
  var stList : List[CacheBoxRO[_]] = List()
  @inline override def sources : List[CacheBoxRO[_]] = stBoxList.unbox
  override protected def boxUpdate() : Unit = {
    val updateList = stBoxList.unbox
    if (updateList != stList) {
      stList.foreach{x => x.removeDependency(this)}
      updateList.foreach{x => x.addDependency(this)}
      stList = updateList
      dirty()
    }
    updateSrcs()
    super.boxUpdate()
  }
  stBoxList.addDependency(this)
}

object CacheDerivedRO {
  def apply[T](cb : CacheBoxRO[_]*)(updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) : CacheBoxRO[T] = new CacheDerivedRO(cb.toList)(updateFunc)
  def apply[T](cb : List[CacheBoxRO[_]])(updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) : CacheBoxRO[T] = new CacheDerivedRO(cb)(updateFunc)
  def apply[T](cbList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T)(implicit owner : CacheBox.Owner, name : sourcecode.Name) : CacheBoxRO[T] = new CacheDerivedROList(cbList)(updateFunc)
}

