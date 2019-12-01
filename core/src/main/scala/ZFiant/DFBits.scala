package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TTokenValue = XBitVector[W]
  override def toString: String = s"DFBits($width)"
}

object DFBits {
  def dfType[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  case class Token[W](dfType : DFBits[W], value : XBitVector[W], bubbleMask : XBitVector[W]) extends DFAny.Token[DFBits[W]]
}