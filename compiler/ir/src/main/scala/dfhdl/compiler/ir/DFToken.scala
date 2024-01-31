package dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp

final case class DFToken[+T <: DFType](dfType: T)(
    val data: dfType.Data
) derives CanEqual:
  val width = dfType.width
  override def equals(that: Any): Boolean = that match
    case that @ DFToken(dfType) if this.dfType equals dfType =>
      that.data equals this.data
    case _ => false
  override def hashCode(): Int = data.hashCode()
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

  def calcFuncOp(op: FuncOp, args: List[DFTokenAny]): DFTokenAny = ???
  // (op, args) match
  //   case (FuncOp.===, lhs :: rhs :: Nil) if !lhs.isBubble && !rhs.isBubble =>
  //     DFToken(DFBool)(Some(lhs.data equals rhs.data))
  //   case (FuncOp.=!=, lhs :: rhs :: Nil) if !lhs.isBubble && !rhs.isBubble =>
  //     DFToken(DFBool)(Some(!(lhs.data equals rhs.data)))
  //   case (FuncOp.+, (lhs @ DFDecimal.Token(_, _)) :: (rhs @ DFDecimal.Token(_, _)) :: Nil) =>
  //     ???

  extension (token: DFToken[?])
    def as[T <: DFType](dfType: T): DFToken[T] =
      val fromType = token.dfType
      val toType = dfType
      val ret = (toType, fromType) match
        // no casting needed
        case (t, f) if t == f => token
        // unsigned to signed conversion
        case (DFSInt(tWidth), DFUInt(fWidth)) =>
          assert(tWidth == fWidth + 1)
          import DFUInt.Ops.signed
          token.asInstanceOf[DFDecimal.Token].signed
        // Bits resize
        case (DFBits(tWidth), DFBits(_)) =>
          import DFBits.Ops.resize
          token.asInstanceOf[DFBits.Token].resize(tWidth)
        // UInt resize
        case (DFUInt(tWidth), DFUInt(_)) =>
          import DFXInt.Ops.resize
          token.asInstanceOf[DFDecimal.Token].resize(tWidth)
        // SInt resize
        case (DFSInt(tWidth), DFSInt(_)) =>
          import DFXInt.Ops.resize
          token.asInstanceOf[DFDecimal.Token].resize(tWidth)
        // Casting from any token to Bits
        case (DFBits(tWidth), _) =>
          assert(tWidth == fromType.width)
          token.bits
        // Casting from Bits to any token
        case (_, DFBits(fWidth)) =>
          assert(fWidth == toType.width)
          DFToken.forced(dfType, dfType.bitsDataToData(token.asInstanceOf[DFBits.Token].data))
        // Casting from any token to any token
        case _ =>
          assert(fromType.width == toType.width)
          DFToken.forced(dfType, dfType.bitsDataToData(token.bits.data))
      ret.asInstanceOf[DFToken[T]]
end DFToken

type DFTokenAny = DFToken[DFType]
