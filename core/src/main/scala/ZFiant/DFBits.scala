package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFBits.Token[W]
  override def toString: String = s"DFBits($width)"
}

object DFBits {
  def dfType[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  final case class Token[W](width : TwoFace.Int[W], value : XBitVector[W], bubbleMask : XBitVector[W]) extends DFAny.Token.Of[XBitVector[W], W] {
    lazy val valueBits : XBitVector[W] = value
  }
}