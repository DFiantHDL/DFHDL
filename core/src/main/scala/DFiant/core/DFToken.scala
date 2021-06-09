package DFiant
package core
import compiler.printing.*
import internals.*

final case class DFToken[+T <: DFType](dfType: T)(val data: dfType.TokenData)
    extends NCCode:
  def ==[R <: DFType](rhs: DFToken[R]): DFBool.Token = ???
  lazy val (valueBits: BitVector, bubbleBits: BitVector) =
    dfType.tokenDataToBits(data)
  def codeString(using Printer): String = dfType.tokenCodeString(data)

object DFToken:
  extension [T <: DFType](token: DFToken[T])
    def width(using w: DFType.Width[T]): Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](token.dfType.__width)
    def bits(using w: DFType.Width[T]): DFToken[DFBits[w.Out]] =
      DFBits.Token(width)(token.valueBits, token.bubbleBits)
