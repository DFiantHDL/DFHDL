package DFiant.internals

trait HasCodeString {
  def codeString : String
}

trait CodeStringOf[T] {
  def apply(t : T) : String
}
object CodeStringOf {
  implicit def ev[T <: HasCodeString] : CodeStringOf[T] = t => t.codeString
  implicit def evBoolean : CodeStringOf[Boolean] = t => t.toString
  implicit def evInt : CodeStringOf[Int] = t => t.toString
  implicit def evLong : CodeStringOf[Long] = t => t.toString
  implicit def evBigInt : CodeStringOf[BigInt] = t => t.codeString
}
