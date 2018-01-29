package DFiant.tokens

import DFiant.internals._
import scodec.bits._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  //maximum token value width
  val width : Int
  final lazy val widthOfValue : Int = math.max(valueBits.lengthOfValue, bubbleMask.lengthOfValue).toInt
  val valueBits : BitVector
  val bubbleMask : BitVector
  //leading zero counter
  final lazy val lzc : Int = math.min(valueBits.lzc, bubbleMask.lzc).toInt
  final def isBubble : Boolean = !(bubbleMask === BitVector.low(width))

  final def bit(relBit : Int) : TokenBool = {
    val outBitsValue = valueBits.bit(relBit)
    val outBubbleMask = bubbleMask.bit(relBit)
    new TokenBool(outBitsValue, outBubbleMask)
  }
  final def bits() : TokenBits = new TokenBits(width, valueBits, bubbleMask)
  final def bits(relBitHigh : Int, relBitLow : Int) : TokenBits = {
    val outWidth = relBitHigh - relBitLow + 1
    val outBitsValue = valueBits.bits(relBitHigh, relBitLow)
    val outBubbleMask = bubbleMask.bits(relBitHigh, relBitLow)
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def bitsWL(relWidth : Int, relBitLow : Int) : TokenBits = bits(relWidth + relBitLow - 1, relBitLow)
  final def == (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBits == that.valueBits)
  }
  final def != (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBits != that.valueBits)
  }

  def bubbleString : String = "Φ"
  def valueString : String = valueBits.toShortString
  override def toString: String = if (isBubble) bubbleString else valueString

  def codeString : String = "<bad codeString>"
}

object Token {
}

object TokenSeq {
  def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
    leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
  def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
    seq.map(t => op(t))
}

