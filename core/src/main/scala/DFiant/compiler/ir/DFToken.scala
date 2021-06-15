package DFiant.compiler.ir

final case class DFToken[+T <: DFType, +D <: Any](dfType : T, data: D):
  val width = dfType.width
