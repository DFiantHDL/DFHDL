import DFiant.internals.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
object InlinedSpec:
  val tf1 = Inlined(1)
  def fromInlined[T <: Int](x: Inlined[T]): Inlined[T] = x
  val tf2 = fromInlined(2)
  val one = 1
  val tfOne: Inlined[one.type] = Inlined(one)
  val res: Inlined[5] = tf1 + tf2 + 2
  val res5 = tfOne + tf2 + 2
  val x: 1 = tf1

  def foo[T1 <: Int, T2 <: Int](
      arg1: Inlined[T1],
      arg2: Inlined[T2]
  ) = (arg1 + arg2) + 2

  val resFoo: Inlined[5] = foo(tf1, tf2)

// positive(-1)
// object conversions:
//   transparent inline given [T <: Int]: ~.Conversion[Positive[_], T] =
//     new ~.Conversion[Positive[_], T]:
//       type To = Positive[T]
//       def apply(from: T): Positive[T] =
//         Inlined(from).asInstanceOf[Positive[T]]

// opaque type Positive[T <: Int] <: Inlined[T] = Inlined[T]
end InlinedSpec
