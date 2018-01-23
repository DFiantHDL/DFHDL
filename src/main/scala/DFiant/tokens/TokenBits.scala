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
    val outValueUInt = BigInt(this.valueBits.toByteArray)
    val outBubble = isBubble
    new TokenUInt(outWidth, outValueUInt, outBubble)
  }
}

object TokenBits {
  def | (left : TokenBits, right : TokenBits) : TokenBits = left | right
  def & (left : TokenBits, right : TokenBits) : TokenBits = left & right
  def ^ (left : TokenBits, right : TokenBits) : TokenBits = left ^ right
  def ## (left : TokenBits, right : TokenBits) : TokenBits = left ## right
  def unary_~ (left : TokenBits) : TokenBits = ~left
  def toUInt(left : TokenBits) : TokenUInt = left.toUInt

  def apply(width : Int, value : Int) : TokenBits = TokenBits(width, BitVector.fromInt(value, width, ByteOrdering.LittleEndian))
  def apply(width : Int, value : Long) : TokenBits = TokenBits(width, BitVector.fromLong(value, width, ByteOrdering.LittleEndian))
  def apply(width : Int, value : BitVector) : TokenBits = {
    //TODO: Boundary checks
    new TokenBits(width, value, BitVector.low(width))
  }
  def apply(width : Int, value : Bubble) : TokenBits = new TokenBits(width, BitVector.low(width), BitVector.high(width))
  def apply(width : Int, value : TokenBits) : TokenBits = {
    //TODO: Boundary checks
    value.bits(width-1, 0)
  }
}

