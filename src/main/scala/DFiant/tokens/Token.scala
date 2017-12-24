package DFiant.tokens

import singleton.twoface._
import DFiant.internals._

sealed trait Bubble
object Bubble extends Bubble

trait Token {
  type Width
  val width : TwoFace.Int[Width]

//  val
  def bits() : TokenBits[Width]
}
object Token {
  def `+`(arg0 : Token, arg1 : Token) : Token = ???
  def apply(value : BigInt) : Token = ???
}

trait TokenW[W] extends Token {
  type Width = W
}



class TokenBits[W] private (val width : TwoFace.Int[W], val validMask : BigInt, val value : BigInt) extends TokenW[W] {
  final def bits() : TokenBits[W] = this
}
object TokenBits {
  implicit def fromInt[W](value : Int) : TokenBits[W] =
    new TokenBits[W]()
  implicit def fromBubble[W](bubble : Bubble) : TokenBits[W] = ???
}

//case object ZeroToken extends TokenBits //TODO