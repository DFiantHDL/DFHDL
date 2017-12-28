package DFiant.tokens

class TokenBool private[DFiant] (val boolValue : Boolean, val bubbleMask : BigInt) extends Token {
  val width : Int = 1
  val bitsValue : BigInt = if (boolValue) 1 else 0

  final def && (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool.fromBubble()
    else TokenBool.fromBoolean(this.boolValue && that.boolValue)
  }
  final def || (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool.fromBubble()
    else TokenBool.fromBoolean(this.boolValue || that.boolValue)
  }
  final def unary_! : TokenBool = {
    if (this.isBubble) TokenBool.fromBubble()
    else TokenBool.fromBoolean(!this.boolValue)
  }
}

object TokenBool {
  def fromBoolean(value : Boolean) : TokenBool = new TokenBool(value, 0)
  def fromBubble() : TokenBool = new TokenBool(false, 1)
}
