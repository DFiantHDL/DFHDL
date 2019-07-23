package DFiant.internals

trait StateBoxRO[T] {
  @inline def get : T
}
object StateBoxRO {
  implicit def toValue[T](sf : StateBoxRO[T]) : T = sf.get
}

class StateBoxRW[T](default : T) extends StateBoxRO[T] {
  private var value : T = default
  @inline def set(newValue : T) : Unit = value = newValue
  @inline def get : T = value
  @inline override def toString: String = value.toString
}
object StateBoxRW {
  def apply[T](t : T) : StateBoxRW[T] = new StateBoxRW[T](t)
}

case class StateDerivedRW[T, R](st : StateBoxRW[T])(t2r : T => R)(r2t : (T, R) => T) extends StateBoxRW[R](t2r(st)) {
  private var oldT : T = st
  @inline override def set(newValue : R) : Unit = {
    val oldR = super.get
    if (newValue != oldR) {
      super.set(newValue)
      val newT = r2t(st.get, newValue)
      st.set(newT)
      oldT = newT
    }
  }
  @inline override def get : R = {
    val newT = st.get
    if (oldT == newT) super.get
    else {
      oldT = newT
      val newR = t2r(newT)
      super.set(newR)
      newR
    }
  }
}

case class StateDerivedRO[T, R](st : StateBoxRO[T])(t2r : T => R) extends StateBoxRO[R] {
  private var oldT : T = st
  private var oldR : R = t2r(oldT)
  @inline override def get : R = {
    val newT = st.get
    if (oldT == newT) oldR
    else {
      oldT = newT
      val newR = t2r(newT)
      oldR = newR
      newR
    }
  }
  @inline override def toString: String = oldR.toString
}