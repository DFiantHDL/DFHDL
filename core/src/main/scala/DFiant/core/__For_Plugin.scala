package DFiant.core

object __For_Plugin:
  def toFunc1[R](block: => R): () => R = () => block
  def toTuple2[T1, T2](t1: T1, t2: T2): (T1, T2) = (t1, t2)
  def fromBoolean(value: Boolean)(using DFC): DFValOf[DFBool] =
    DFVal.Const(DFBoolOrBit.Token(DFBool, value))
