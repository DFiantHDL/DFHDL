package DFiant.tokens

import DFiant.internals._
import scodec.bits._

class TokenBits private[DFiant] (val width : Int, val getBitsValue : BitVector, val getBubbleMask : BitVector) extends Token {
  final def | (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.getBitsValue | that.getBitsValue
    val outBubbleMask = this.getBubbleMask | that.getBubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def & (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.getBitsValue & that.getBitsValue
    val outBubbleMask = this.getBubbleMask | that.getBubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ^ (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.getBitsValue ^ that.getBitsValue
    val outBubbleMask = this.getBubbleMask | that.getBubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ## (that : TokenBits) : TokenBits = {
    val outWidth = this.width + that.width
    val outBitsValue = this.getBitsValue ++ that.getBitsValue
    val outBubbleMask = this.getBubbleMask ++ that.getBubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def unary_~ : TokenBits = {
    val outWidth = this.width
    val outBitsValue = ~this.getBitsValue
    val outBubbleMask = this.getBubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
}

object TokenBits {
  def | (left : TokenBits, right : TokenBits) : TokenBits = left | right
  def & (left : TokenBits, right : TokenBits) : TokenBits = left & right
  def ^ (left : TokenBits, right : TokenBits) : TokenBits = left ^ right
  def ## (left : TokenBits, right : TokenBits) : TokenBits = left ## right
  def unary_~ (left : TokenBits) : TokenBits = ~left

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

