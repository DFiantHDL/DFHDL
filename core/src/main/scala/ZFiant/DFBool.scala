package ZFiant

import singleton.twoface._
import DFiant.internals._

class DFBool extends DFType {
  type Width = 1
  type TToken = DFBool.Token
  val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
  override def toString: String = "DFBool()"
}

object DFBool {
  def dfType() = new DFBool()
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(), Seq())

  final case class Token(value : Boolean, bubble : Boolean) extends DFAny.Token.Of[Boolean, 1] {
    val width: TwoFace.Int[1] = 1
    lazy val valueBits : XBitVector[1] = XBitVector.bit(value)
    lazy val bubbleMask: XBitVector[1] = XBitVector.bit(bubble)
  }

  object Token {
    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(valueBool : Boolean, bubble : Boolean) : Token = new Token(valueBool, bubble)
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)

  }

}

