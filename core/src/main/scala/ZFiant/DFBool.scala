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

}

