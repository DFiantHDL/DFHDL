package DFiant.compiler.ir

final case class DFToken[+T <: DFType, +D <: Any](dfType: T, data: D)
    derives CanEqual:
  val width = dfType.width

type DFTokenAny = DFToken[DFType, Any]
