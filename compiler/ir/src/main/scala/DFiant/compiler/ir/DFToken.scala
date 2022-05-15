package dfhdl.compiler.ir

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

  extension (token: DFTokenAny)
    def bits: DFBits.Token = DFToken.forced(
      DFBits(token.width),
      token.dfType.dataToBitsData(token.data)
    )
    def isBubble: Boolean =
      token.dfType.isDataBubble(token.data)
  end extension

  extension (token: DFBits.Token)
    def as(dfType: DFType): DFTokenAny =
      DFToken.forced(dfType, dfType.bitsDataToData(token.data))
end DFToken

type DFTokenAny = DFToken[DFType]
