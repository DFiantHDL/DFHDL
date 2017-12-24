package DFiant.tokens

import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val bitsValue : BigInt
  val bubbleMask : BigInt
}
object Token {
  def `+`(arg0 : Token, arg1 : Token) : Token = ???
  def apply(value : BigInt) : Token = ???
}

trait TokenOf[DFVal] extends Token


class TokenBits private (val bitsValue : BigInt, val bubbleMask : BigInt) extends Token {
}
object TokenBits {
  def fromInt(value : Int) : TokenBits =
    new TokenBits(value, 0)
  implicit def fromBubble(bubble : Bubble) : TokenBits =
    new TokenBits(0, 1)
  BigInt(0).signum
}

//case object ZeroToken extends TokenBits //TODO