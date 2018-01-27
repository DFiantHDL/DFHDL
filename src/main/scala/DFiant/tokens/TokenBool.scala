package DFiant.tokens

import scodec.bits._

class TokenBool private[DFiant] (val valueBool : Boolean, val bubble : Boolean) extends Token {
  val width : Int = 1
  lazy val valueBits : BitVector = BitVector.bit(valueBool)
  lazy val bubbleMask: BitVector = BitVector.bit(bubble)

  final def && (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBool && that.valueBool)
  }
  final def || (that : TokenBool) : TokenBool = {
    if (this.isBubble || that.isBubble) TokenBool(Bubble)
    else TokenBool(this.valueBool || that.valueBool)
  }
  final def unary_! : TokenBool = {
    if (this.isBubble) TokenBool(Bubble)
    else TokenBool(!this.valueBool)
  }
  override def valueString : String = valueBool.toString()
}

object TokenBool {
  def || (left : Seq[TokenBool], right : Seq[TokenBool]) : Seq[TokenBool] = TokenSeq(left, right)((l, r) => l || r)
  def && (left : Seq[TokenBool], right : Seq[TokenBool]) : Seq[TokenBool] = TokenSeq(left, right)((l, r) => l && r)
  def unary_! (left : Seq[TokenBool]) : Seq[TokenBool] = TokenSeq(left)(t => !t)

  def apply(value : Int) : TokenBool = value match {
    case 0 => TokenBool(false)
    case 1 => TokenBool(true)
  }
  def apply(value : Boolean) : TokenBool = new TokenBool(value, false)
  def apply(value : Bubble) : TokenBool = new TokenBool(false, true)
}
