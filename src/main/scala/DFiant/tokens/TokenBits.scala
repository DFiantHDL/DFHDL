package DFiant.tokens

import DFiant.internals._

class TokenBits private[DFiant] (val width : Int, val bitsValue : BigInt, val bubbleMask : BigInt) extends Token {
  final def + (that : TokenBits) : TokenBits = { //TODO: There is no `+` for Bits operations
    val outWidth = math.max(this.width, that.width) + 1
    if (this.isBubble || that.isBubble) TokenBits(outWidth, Bubble)
    else TokenBits(outWidth, this.bitsValue + that.bitsValue)
  }
  final def | (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.bitsValue | that.bitsValue
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def & (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.bitsValue & that.bitsValue
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ^ (that : TokenBits) : TokenBits = {
    val outWidth = math.max(this.width, that.width)
    val outBitsValue = this.bitsValue ^ that.bitsValue
    val outBubbleMask = this.bubbleMask | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def ## (that : TokenBits) : TokenBits = {
    val outWidth = this.width + that.width
    val outBitsValue = this.bitsValue << that.width | that.bitsValue
    val outBubbleMask = this.bubbleMask << that.width | that.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
  final def unary_~ : TokenBits = {
    val outWidth = this.width
    val outBitsValue = ~this.bitsValue
    val outBubbleMask = this.bubbleMask
    new TokenBits(outWidth, outBitsValue, outBubbleMask)
  }
}

object TokenBits {
  def + (left : TokenBits, right : TokenBits) : TokenBits = left + right
  def | (left : TokenBits, right : TokenBits) : TokenBits = left | right
  def & (left : TokenBits, right : TokenBits) : TokenBits = left & right
  def ^ (left : TokenBits, right : TokenBits) : TokenBits = left ^ right
  def ## (left : TokenBits, right : TokenBits) : TokenBits = left ## right

  def apply(width : Int, value : Int) : TokenBits = TokenBits(width, BigInt(value))
  def apply(width : Int, value : Long) : TokenBits = TokenBits(width, BigInt(value))
  def apply(width : Int, value : BigInt) : TokenBits = {
    //TODO: Boundary checks
    new TokenBits(width, value, 0)
  }
  def apply(width : Int, value : Bubble) : TokenBits = new TokenBits(width, 0, bitsWidthToMaxBigIntBits(width))
  def apply(width : Int, value : TokenBits) : TokenBits = {
    //TODO: Boundary checks
    value.bits(width-1, 0)
  }
}

