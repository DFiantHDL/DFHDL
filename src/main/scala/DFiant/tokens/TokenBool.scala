package DFiant.tokens

import scodec.bits._

class TokenBool private[DFiant] (val boolValue : Boolean, val bubble : Boolean) extends Token {
  val width : Int = 1
  def getBitsValue : BitVector = BitVector.bit(boolValue)
  def getBubbleMask: BitVector = BitVector.bit(bubble)

  final def && (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.boolValue && that.boolValue)
  }
  final def || (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.boolValue || that.boolValue)
  }
  final def unary_! : TokenBool = {
    if (this.isBubble) TokenBool(Bubble)
    else TokenBool(!this.boolValue)
  }
}

object TokenBool {
  def || (left : TokenBool, right : TokenBool) : TokenBool = left || right
  def && (left : TokenBool, right : TokenBool) : TokenBool = left && right
  def unary_! (left : TokenBool) : TokenBool = !left

  def apply(value : Int) : TokenBool = value match {
    case 0 => TokenBool(false)
    case 1 => TokenBool(true)
  }
  def apply(value : Boolean) : TokenBool = new TokenBool(value, false)
  def apply(value : Bubble) : TokenBool = new TokenBool(false, true)
}
