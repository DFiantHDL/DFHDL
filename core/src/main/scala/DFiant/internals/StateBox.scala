package DFiant.internals

trait StateBoxRO[T] {
  @inline def get : T
  @inline final override def toString: String = get.toString
}
object StateBoxRO {
  implicit def toValue[T](sf : StateBoxRO[T]) : T = sf.get
}

class StateConst[T](default : => T) extends StateBoxRO[T] {
  @inline def get : T = default
}
object StateConst {
  def apply[T](default : => T) : StateConst[T] = new StateConst[T](default)
}

class StateBoxRW[T](default : T) extends StateBoxRO[T] {
  private var value : T = default
  @inline def set(newValue : T) : Unit = value = newValue
  @inline def get : T = value
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

class StateDerivedRO[T, R](ft : => T)(t2r : T => R) extends StateBoxRO[R] {
  private var oldTR : Option[(T, R)] = None
  @inline override def get : R = {
    val newT = ft
    oldTR match {
      case Some((t, r)) if t == newT => r
      case _ =>
        val newR = t2r(newT)
        oldTR = Some((newT, newR))
        newR
    }
  }
}
object StateDerivedRO {
  def apply[T, R](st : StateBoxRO[T])(t2r : T => R) : StateDerivedRO[T, R] = new StateDerivedRO(st.get)(t2r)
  def apply[T, R](st : => T)(t2r : T => R) : StateDerivedRO[T, R] = new StateDerivedRO(st)(t2r)
}