package DFiant.tokens

import DFiant.internals._

class TokenBits private[DFiant] (val width : Int, val bitsValue : BigInt, val bubbleMask : BigInt) extends Token {
  final def + (that : TokenBits) : TokenBits = { //TODO: There is no `+` for Bits operations
    val outWidth = math.max(this.width, that.width) + 1
    if (this.isBubble || that.isBubble) TokenBits.fromBubble(outWidth)
    else TokenBits.fromNum(outWidth, this.bitsValue + that.bitsValue)
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
  //Bit concatenation required additional width information
  //  def ##(leftToken : TokenBits, leftWidth : Int, rightToken : TokenBits, rightWidth : Int) : TokenBits = ???

  def fromNum(width : Int, value : Int) : TokenBits = fromNum(width, BigInt(value))
  def fromNum(width : Int, value : Long) : TokenBits = fromNum(width, BigInt(value))
  def fromNum(width : Int, value : BigInt) : TokenBits = new TokenBits(width, value, 0)
  def fromBubble(width : Int) : TokenBits = new TokenBits(width, 0, bitsWidthToMaxBigIntBits(width))
}