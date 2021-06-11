//package DFiant
//package core
//import compiler.printing.*
//import internals.*
//
//final case class DFToken[+T <: DFType](dfType: T)(val data: dfType.TokenData)
//    extends NCCode:
//  def ==[R <: DFType](rhs: DFToken[R]): DFBool.Token =
//    dfType.tokenEquals(this, rhs)
//  protected[DFiant] lazy val (valueBits: BitVector, bubbleBits: BitVector) =
//    dfType.tokenDataToBits(data)
//  def codeString(using Printer): String = dfType.tokenCodeString(data)
//
//object DFToken:
//  extension [T <: DFType](token: DFToken[T])(using w: Width[T])
//    def width: Inlined.Int[w.Out] =
//      Inlined.Int.forced[w.Out](token.dfType.__width)
//    def bits: DFToken[DFBits[w.Out]] =
//      DFBits.Token(width)(token.valueBits, token.bubbleBits)
//
//  extension [W <: Int](token: DFBits.Token[W])
//    def as[T <: DFType](dfType: T)(using w: Width[T]): DFToken[T] =
//      DFToken(dfType)(dfType.tokenBitsToData(token.valueBits, token.bubbleBits))
