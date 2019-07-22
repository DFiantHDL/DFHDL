package DFiant.internals

class StateFull[T](default : T) {
  private var value : T = default
  @inline def set(newValue : T) : Unit = value = newValue
  @inline def get : T = value
  @inline override def toString: String = value.toString
}
object StateFull {
  implicit def toValue[T](sf : StateFull[T]) : T = sf.get
  def apply[T](t : T) : StateFull[T] = new StateFull[T](t)
}

case class StateDerived[T, R](st : StateFull[T])(t2r : T => R)(r2t : (T, R) => T) extends StateFull[R](t2r(st)) {
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