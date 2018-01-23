package DFiant.tokens

import DFiant.internals._
import scodec.bits._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  //maximum token value width
  val width : Int
  final lazy val widthOfValue : Int = if (lzc == width) 1 else width - lzc
  val valueBits : BitVector
  val bubbleMask : BitVector
  //leading zero counter
  final lazy val lzc : Int = {
    val l = for (i <- 0 until width if valueBits(i) || bubbleMask(i)) yield i
    if (l.isEmpty) width else l.head
  }
  final def isBubble : Boolean = !(bubbleMask === BitVector.low(width))
  protected def ri(bitIdx : Int) : Int = width - 1 - bitIdx //reverse index for BitVector

  final def bit(relBit : Int) : TokenBool = {
    val riRelBit = ri(relBit)
    val outBitsValue = valueBits(riRelBit)
    val outBubbleMask = bubbleMask(riRelBit)
    new TokenBool(outBitsValue, outBubbleMask)
  }
  final def bits() : TokenBits = new TokenBits(width, valueBits, bubbleMask)
  final def bits(relBitHigh : Int, relBitLow : Int) : TokenBits = {
    val outWidth = relBitHigh - relBitLow + 1
    val riRelBitHigh = ri(relBitHigh)
    val riRelBitLow = ri(relBitLow)
    val outBitsValue = valueBits.slice(riRelBitLow, riRelBitHigh + 1)
    val outBubbleMask = bubbleMask.slice(riRelBitLow, riRelBitHigh + 1)
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def == (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBits == that.valueBits)
  }
  final def != (that : this.type) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBits != that.valueBits)
  }

  def bubbleString : String = "Φ"
  def valueString : String = {
    val nibble = 4
    //narrowing the vector by removing all the leftest zeros
    val narrowVec = valueBits.takeRight(widthOfValue)
    val paddedVecWidth = ((widthOfValue + nibble - 1) / nibble) * nibble
    //default printing of bitvectors is padding-right in `toHex`.
    //padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    s"0x${narrowVec.padLeft(paddedVecWidth).toHex}"
  }
  override def toString: String = if (isBubble) bubbleString else valueString
}

object Token {
}


abstract class TokenSeq[T <: Token](seq : Seq[T]) {
  def applyOp[B <: Token, T2 <: Token](that : Seq[T2], op : (T, T2) => B) : Seq[B] =
    seq.zipAll(that.seq, seq.last, that.seq.last).map(t => op(t._1, t._2))
  def applyOp[B <: Token](op : T => B) : Seq[B] =
    seq.map(t => op(t))
}

