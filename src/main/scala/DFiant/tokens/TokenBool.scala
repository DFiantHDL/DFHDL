package DFiant.tokens

class TokenBool private[DFiant] (val boolValue : Boolean, val bubbleMask : BigInt) extends Token {
  val width : Int = 1
  val bitsValue : BigInt = if (boolValue) 1 else 0

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
  def apply(value : Boolean) : TokenBool = new TokenBool(value, 0)
  def apply(value : Bubble) : TokenBool = new TokenBool(false, 1)
}
