package DFiant.tokens

import DFiant.internals._
import scodec.bits._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val width : Int
  def getBitsValue : BitVector
  def getBubbleMask : BitVector
  final def isBubble : Boolean = !(getBubbleMask === BitVector.zero)
  protected def ri(bitIdx : Int) : Int = width - 1 - bitIdx //reverse index for BitVector

  final def bit(relBit : Int) : TokenBool = {
    val riRelBit = ri(relBit)
    val outBitsValue = getBitsValue(riRelBit)
    val outBubbleMask = getBubbleMask(riRelBit)
    new TokenBool(outBitsValue, outBubbleMask)
  }
  final def bits() : TokenBits = new TokenBits(width, getBitsValue, getBubbleMask)
  final def bits(relBitHigh : Int, relBitLow : Int) : TokenBits = {
    val outWidth = relBitHigh - relBitLow + 1
    val riRelBitHigh = ri(relBitHigh)
    val riRelBitLow = ri(relBitLow)
    val outBitsValue = getBitsValue.slice(riRelBitLow, riRelBitHigh + 1)
    val outBubbleMask = getBubbleMask.slice(riRelBitLow, riRelBitHigh + 1)
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def == (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.getBitsValue == that.getBitsValue)
  }
  final def != (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.getBitsValue != that.getBitsValue)
  }

  override def toString: String = if (isBubble) "Φ" else s"0x${getBitsValue.toHex}"
}

object Token {
}


abstract class TokenSeq[T <: Token](seq : Seq[T]) {
  def applyOp[B <: Token, T2 <: Token](that : Seq[T2], op : (T, T2) => B) : Seq[B] =
    seq.zipAll(that.seq, seq.last, that.seq.last).map(t => op(t._1, t._2))
  def applyOp[B <: Token](op : (T) => B) : Seq[B] =
    seq.map(t => op(t))
}

