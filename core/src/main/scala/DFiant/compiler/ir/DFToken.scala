package DFiant.compiler.ir

final case class DFToken(dfType : DFType, data: Any):
  val width = dfType.width
