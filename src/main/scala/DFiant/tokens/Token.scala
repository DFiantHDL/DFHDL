package DFiant.tokens

import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val width : Int
  val bitsValue : BigInt
  val bubbleMask : BigInt

  final def bits() : TokenBits = new TokenBits(width, bitsValue, bubbleMask)
}

object Token {
  def `+`(arg0 : Token, arg1 : Token) : Token = ???
  def apply(value : BigInt) : Token = ???
}


class TokenBits private[DFiant] (val width : Int, val bitsValue : BigInt, val bubbleMask : BigInt) extends Token {
  def +(that : TokenBits) : TokenBits = ???
}
object TokenBits {
  //Bit concatenation required additional width information
//  def ##(leftToken : TokenBits, leftWidth : Int, rightToken : TokenBits, rightWidth : Int) : TokenBits = ???

  def fromInt(width : Int, value : Int) : TokenBits =
    new TokenBits(width, value, 0)
  implicit def fromBubble(width : Int, bubble : Bubble) : TokenBits =
    new TokenBits(width, 0, 1)
}

//case object ZeroToken extends TokenBits //TODO