package DFiant.internals
import compiletime.ops.int.*
protected object std:
  type Int     = scala.Int
  type Long    = scala.Long
  type Float   = scala.Float
  type Double  = scala.Double
  type String  = java.lang.String
  type Boolean = scala.Boolean

type XInt     = Int with Singleton
type XLong    = Long with Singleton
type XFloat   = Float with Singleton
type XDouble  = Double with Singleton
type XString  = String with Singleton
type XBoolean = Boolean with Singleton

class Inlined[T <: Wide, Wide](val value: Wide) extends AnyVal {
  type Out = T
  override def equals(obj: Any): Boolean = obj match {
    case right: Inlined[_, _] => value equals right.value
    case _                    => false
  }
  override def toString: String = value.toString
}

object Inlined:
  implicit def getValue[T <: Wide, Wide](inlined: Inlined[T, Wide]): Wide =
    inlined.value
  inline implicit def fromValue[Wide, T <: Wide](
      inline value: T
  ): Inlined[value.type, Wide] = Inlined[value.type, Wide](value)
  protected trait Companion[Wide]:
    def forced[T <: Wide](value: Wide)   = Inlined[T, Wide](value)
    inline def apply(inline value: Wide) = Inlined[value.type, Wide](value)

  type Int[T <: std.Int] = Inlined[T, std.Int]
  object Int extends Companion[std.Int]
  extension [T <: std.Int](lhs: Int[T])
    def +[R <: std.Int](rhs: Int[R]) = new Int[T + R](lhs.value + rhs.value)

  val tf1         = Int(1)
  val tf2         = Int(2)
  val one         = 1
  val tfOne       = Int(one)
  val res: Int[5] = tf1 + tf2 + 2
  val res5        = tfOne + tf2 + 2

  def foo[T1 <: std.Int, T2 <: std.Int](
      arg1: Int[T1],
      arg2: Int[T2]
  ) = (arg1 + arg2) + 2

  val resFoo: Int[5] = foo(tf1, tf2)
