package DFiant.tokens

import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val width : Int
  val bitsValue : BigInt
  val bubbleMask : BigInt
  final def isBubble : Boolean = bubbleMask != 0

  final def bit(relBit : Int) : TokenBool = {
    val outBitsValue = if (bitsValue.testBit(relBit)) true else false
    val outBubbleMask = bitsSel(bubbleMask, relBit, relBit)
    new TokenBool(outBitsValue, outBubbleMask)
  }
  final def bits() : TokenBits = new TokenBits(width, bitsValue, bubbleMask)
  final def bits(relBitHigh : Int, relBitLow : Int) : TokenBits = {
    val outWidth = relBitHigh - relBitLow + 1
    val outBitsValue = bitsSel(bitsValue, relBitHigh, relBitLow)
    val outBubbleMask = bitsSel(bubbleMask, relBitHigh, relBitLow)
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def == (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.bitsValue == that.bitsValue)
  }
  final def != (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.bitsValue != that.bitsValue)
  }

  override def toString: String = if (isBubble) "Φ" else s"0x${bitsValue.toString(16)}"
}

object Token {
}


abstract class TokenSeq[T <: Token](seq : Seq[T]) {
  def applyOp[B <: Token, T2 <: Token](that : Seq[T2], op : (T, T2) => B) : Seq[B] =
    seq.zipAll(that.seq, seq.last, that.seq.last).map(t => op(t._1, t._2))
  def applyOp[B <: Token](op : (T) => B) : Seq[B] =
    seq.map(t => op(t))
}

