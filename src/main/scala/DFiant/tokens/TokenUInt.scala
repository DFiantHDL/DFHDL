package DFiant.tokens

import DFiant.internals._
import scodec.bits._

class TokenUInt private[DFiant] (val width : Int, val valueUInt : BigInt, val bubble : Boolean) extends Token {
  lazy val valueBits : BitVector = BitVector(valueUInt.toByteArray).toLength(width)
  lazy val bubbleMask: BitVector = BitVector.fill(width)(bubble)
  def mkTokenU(that : TokenUInt, result : BigInt, resultWidth : Int) : TokenUInt = {
    if (this.isBubble || that.isBubble) TokenUInt(resultWidth, Bubble)
    else TokenUInt(resultWidth, result.asUnsigned)
  }

  final def + (that : TokenUInt) : TokenUInt = mkTokenU(that, this.valueUInt + that.valueUInt, math.max(this.width, that.width) + 1)
  final def - (that : TokenUInt) : TokenUInt = mkTokenU(that, this.valueUInt - that.valueUInt, math.max(this.width, that.width) + 1)
  final def * (that : TokenUInt) : TokenUInt = mkTokenU(that, this.valueUInt * that.valueUInt, this.width + that.width)
  final def / (that : TokenUInt) : TokenUInt = mkTokenU(that, this.valueUInt / that.valueUInt, this.width)
  final def % (that : TokenUInt) : TokenUInt = mkTokenU(that, this.valueUInt % that.valueUInt, that.width)
  final def <  (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt < that.valueUInt, this.isBubble || that.isBubble)
  final def >  (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt > that.valueUInt, this.isBubble || that.isBubble)
  final def <= (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt <= that.valueUInt, this.isBubble || that.isBubble)
  final def >= (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt >= that.valueUInt, this.isBubble || that.isBubble)
  final def == (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt == that.valueUInt, this.isBubble || that.isBubble)
  final def != (that : TokenUInt) : TokenBool = new TokenBool(this.valueUInt != that.valueUInt, this.isBubble || that.isBubble)

  //  final def unary_- : TokenSInt = {
  //    val outWidth = this.width+1
  //    val outUIntValue = ~this.bitsValue
  //    val outBubbleMask = this.bubbleMask
  //    new TokenUInt(outWidth, outUIntValue, outBubbleMask)
  //  }
  override def codeString: String = if (isBubble) "Φ" else valueUInt.codeString
  override def valueString : String = valueUInt.toString()
}

object TokenUInt {
  def + (left : TokenUInt, right : TokenUInt) : TokenUInt = left + right
  def - (left : TokenUInt, right : TokenUInt) : TokenUInt = left - right
  def * (left : TokenUInt, right : TokenUInt) : TokenUInt = left * right
  def / (left : TokenUInt, right : TokenUInt) : TokenUInt = left / right
  def % (left : TokenUInt, right : TokenUInt) : TokenUInt = left % right
  def < (left : TokenUInt, right : TokenUInt) : TokenBool = left < right
  def > (left : TokenUInt, right : TokenUInt) : TokenBool = left > right
  def <= (left : TokenUInt, right : TokenUInt) : TokenBool = left <= right
  def >= (left : TokenUInt, right : TokenUInt) : TokenBool = left >= right
  def == (left : TokenUInt, right : TokenUInt) : TokenBool = left == right
  def != (left : TokenUInt, right : TokenUInt) : TokenBool = left != right
//  def unary_- (left : TokenUInt) : TokenUInt = -left

  def apply(width : Int, value : Int) : TokenUInt = TokenUInt(width, BigInt(value))
  def apply(width : Int, value : Long) : TokenUInt = TokenUInt(width, BigInt(value))
  def apply(width : Int, value : BigInt) : TokenUInt = {
    if (value < 0 ) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
    new TokenUInt(width, value, false)
  }
  def apply(width : Int, value : Bubble) : TokenUInt = new TokenUInt(width, 0, true)
  def apply(width : Int, token : TokenUInt) : TokenUInt = {
    //TODO: Boundary checks
    new TokenUInt(width, token.valueUInt, token.bubble)
  }
}

