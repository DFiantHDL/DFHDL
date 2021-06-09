import DFiant.internals.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
object InlinedSpec {
  val tf1 = Inlined.Int(1)
  val tf2 = Inlined.Int(2)
  val one = 1
  val tfOne: Inlined.Int[Int] = Inlined.Int(one)
  val res: Inlined.Int[5] = tf1 + tf2 + 2
  val res5 = tfOne + tf2 + 2
  val x: 1 = tf1

  def foo[T1 <: Int, T2 <: Int](
      arg1: Inlined.Int[T1],
      arg2: Inlined.Int[T2]
  ) = (arg1 + arg2) + 2

  val resFoo: Inlined.Int[5] = foo(tf1, tf2)

  // positive(-1)
  // object conversions:
  //   transparent inline given [T <: Int]: ~.Conversion[Positive[_], T] =
  //     new ~.Conversion[Positive[_], T]:
  //       type To = Positive[T]
  //       def apply(from: T): Positive[T] =
  //         Inlined.Int(from).asInstanceOf[Positive[T]]

  // opaque type Positive[T <: Int] <: Inlined.Int[T] = Inlined.Int[T]

}
