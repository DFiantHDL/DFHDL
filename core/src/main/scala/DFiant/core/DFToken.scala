package DFiant
package core
import compiler.printing.*
import internals.*

final case class DFToken[+T <: DFType](dfType: T)(val data: dfType.TokenData)
    extends NCCode:
  lazy val (valueBits: BitVector, bubbleBits: BitVector) =
    dfType.tokenDataToBits(data)
  def codeString(using Printer): String = dfType.tokenCodeString(data)

object DFToken:
  extension [T <: DFType](token: DFToken[T])
    def _width(using w: DFType.Width[T]): Inlined.Int[w.Out] =
      token.dfType.width
    def bits(using w: DFType.Width[T]): DFToken[DFBits[w.Out]] =
      val bitsType = DFBits(_width)
      DFToken(bitsType)((token.valueBits, token.bubbleBits))
