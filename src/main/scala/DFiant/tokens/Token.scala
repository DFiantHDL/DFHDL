package DFiant.tokens

import DFiant.core.DFAny
import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  val width : Int
  val bitsValue : BigInt
  val bubbleMask : BigInt

//  final def bitsOf[Val <: DFAny]() : TokenBitsOf[Val] = new TokenBitsOf[Val](width, bitsValue, bubbleMask)
  override def toString: String = super.toString
}

object Token {
  def `+`(arg0 : Token, arg1 : Token) : Token = ???
  def apply(value : BigInt) : Token = ???
}

trait TokenOf[Val <: DFAny] extends Token {
//  final def bits() : TokenBitsOf[Val] = bitsOf[Val]()
}


class TokenBitsOf[Val <: DFAny] private[DFiant] (val width : Int, val bitsValue : BigInt, val bubbleMask : BigInt) extends TokenOf[Val] {
  def +(that : TokenBitsOf[Val]) : TokenBitsOf[Val] = ???
}
object TokenBits {
  //Bit concatenation required additional width information
//  def ##(leftToken : TokenBits, leftWidth : Int, rightToken : TokenBits, rightWidth : Int) : TokenBits = ???

  def fromInt[Val <: DFAny](width : Int, value : Int) : TokenBitsOf[Val] =
    new TokenBitsOf[Val](width, value, 0)
  implicit def fromBubble[Val <: DFAny](width : Int, bubble : Bubble) : TokenBitsOf[Val] =
    new TokenBitsOf[Val](width, 0, 1)
}

//case object ZeroToken extends TokenBits //TODO