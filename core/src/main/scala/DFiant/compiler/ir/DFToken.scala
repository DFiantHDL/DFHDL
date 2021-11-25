package DFiant.compiler.ir

final case class DFToken[+T <: DFType](dfType: T)(
    val data: dfType.Data
) derives CanEqual:
  val width = dfType.width
  override def equals(that: Any): Boolean = that match
    case that @ DFToken(dfType) if this.dfType equals dfType =>
      that.data equals this.data
    case _ => false
  override def toString: String = s"DFToken($dfType)($data)"

object DFToken:
  def forced[T <: DFType](dfType: T, data: Any) =
    DFToken[T](dfType)(data.asInstanceOf[dfType.Data])
  def bubble(dfType: DFType): DFTokenAny =
    DFToken.forced(dfType, dfType.createBubbleData)
type DFTokenAny = DFToken[DFType]
