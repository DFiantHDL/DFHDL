package DFiant.internals

trait Check1[
    Wide,
    Cond[T <: Wide] <: Boolean,
    Msg[T <: Wide] <: String
]:
  trait Check[T <: Wide]:
    def apply(t: Wide): Unit

trait Check2[
    Wide1,
    Wide2,
    Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
    Msg[T1 <: Wide1, T2 <: Wide2] <: String
]:
  trait Check[T1 <: Wide1, T2 <: Wide2]:
    def apply(t1: Wide1, t2: Wide2): Unit

object RequirePositive
    extends Check1[
      Int,
      [t <: Int] =>> t > 0,
      [t <: Int] =>> "Must be positive, but found: " + t
    ]
