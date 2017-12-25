package DFiant.tokens

import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val width : Int
  val bitsValue : BigInt
  val bubbleMask : BigInt

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
    if (this.bubbleMask != 0 || that.bubbleMask != 0) TokenBool.fromBubble()
    else TokenBool.fromBoolean(this.bitsValue == that.bitsValue)
  }
  final def != (that : this.type) : TokenBool = {
    if (this.bubbleMask != 0 || that.bubbleMask != 0) TokenBool.fromBubble()
    else TokenBool.fromBoolean(this.bitsValue != that.bitsValue)
  }
}

object Token {
//  def `+`(arg0 : Token, arg1 : Token) : Token = ???
//  def apply(value : BigInt) : Token = ???
}

