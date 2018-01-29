package DFiant.tokens

import DFiant.internals._
import scodec.bits._

class TokenBits private[DFiant] (val width : Int, val valueBits : BitVector, val bubbleMask : BitVector) extends Token {
  final def | (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.valueBits | that.valueBits
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def & (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.valueBits & that.valueBits
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ^ (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.valueBits ^ that.valueBits
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ## (that : TokenBits) : TokenBits = {
    val outWidth = this.width + that.width
    val outBitsValue = this.valueBits ++ that.valueBits
    val outBubbleMask = this.bubbleMask ++ that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def unary_~ : TokenBits = {
    val outWidth = this.width
    val outBitsValue = ~this.valueBits
    val outBubbleMask = this.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  def toUInt : TokenUInt = {
    val outWidth = this.width
    val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
    val outBubble = isBubble
    new TokenUInt(outWidth, outValueUInt, outBubble)
  }
}

object TokenBits {
  def | (left : Seq[TokenBits], right : Seq[TokenBits]) : Seq[TokenBits] = TokenSeq(left, right)((l, r) => l | r)
  def & (left : Seq[TokenBits], right : Seq[TokenBits]) : Seq[TokenBits] = TokenSeq(left, right)((l, r) => l & r)
  def ^ (left : Seq[TokenBits], right : Seq[TokenBits]) : Seq[TokenBits] = TokenSeq(left, right)((l, r) => l ^ r)
  def ## (left : Seq[TokenBits], right : Seq[TokenBits]) : Seq[TokenBits] = TokenSeq(left, right)((l, r) => l ## r)
  def unary_~ (left : Seq[TokenBits]) : Seq[TokenBits] = TokenSeq(left)(t => ~t)
  def toUInt(left : Seq[TokenBits]) : Seq[TokenUInt] = TokenSeq(left)(t => t.toUInt)

  def apply(width : Int, value : Int) : TokenBits = TokenBits(width, BitVector.fromInt(value, width))
  def apply(width : Int, value : Long) : TokenBits = TokenBits(width, BitVector.fromLong(value, width))
  def apply(width : Int, value : BitVector) : TokenBits = {
    //TODO: Boundary checks
    new TokenBits(width, value.toLength(width), BitVector.low(width))
  }
  def apply(width : Int, value : Bubble) : TokenBits = new TokenBits(width, BitVector.low(width), BitVector.high(width))
  def apply(width : Int, value : TokenBits) : TokenBits = {
    //TODO: Boundary checks
    value.bits(width-1, 0)
  }
}

