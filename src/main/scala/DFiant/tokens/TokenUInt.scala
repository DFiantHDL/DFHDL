package DFiant.tokens

import DFiant.internals._

class TokenUInt private[DFiant] (val width : Int, val bitsValue : BigInt, val bubbleMask : BigInt) extends Token {
  final def + (that : TokenUInt) : TokenUInt = { //TODO: There is no `+` for UInt operations
    val outWidth = math.max(this.width, that.width) + 1
    if (this.isBubble || that.isBubble) TokenUInt(outWidth, Bubble)
    else TokenUInt(outWidth, this.bitsValue + that.bitsValue)
  }
//  final def unary_- : TokenSInt = {
//    val outWidth = this.width+1
//    val outUIntValue = ~this.bitsValue
//    val outBubbleMask = this.bubbleMask
//    new TokenUInt(outWidth, outUIntValue, outBubbleMask)
//  }
}

object TokenUInt {
  def + (left : TokenUInt, right : TokenUInt) : TokenUInt = left + right
//  def unary_- (left : TokenUInt) : TokenUInt = -left

  def apply(width : Int, value : Int) : TokenUInt = TokenUInt(width, BigInt(value))
  def apply(width : Int, value : Long) : TokenUInt = TokenUInt(width, BigInt(value))
  def apply(width : Int, value : BigInt) : TokenUInt = {
    //TODO: Boundary checks
    new TokenUInt(width, value, 0)
  }
  def apply(width : Int, value : Bubble) : TokenUInt = new TokenUInt(width, 0, bitsWidthToMaxBigIntBits(width))
//  def apply(width : Int, value : TokenUInt) : TokenUInt = {
//    //TODO: Boundary checks
//    value.bits(width-1, 0)
//  }
}

