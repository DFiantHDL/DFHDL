package DFiant.compiler.ir

final case class DFToken[+T <: DFType](dfType: T)(
    val data: dfType.Data
) derives CanEqual:
  val width = dfType.width
  override def equals(that: Any): Boolean = that match
    case that @ DFToken(dfType) if this.dfType equals dfType =>
      that.data equals this.data
    case _ => false

object DFToken:
  def forced[T <: DFType](dfType: T, data: Any) =
    DFToken[T](dfType)(data.asInstanceOf[dfType.Data])
type DFTokenAny = DFToken[DFType]
