package ZFiant

import singleton.ops._
import singleton.twoface._

class DFUInt[W] private (val width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TTokenValue = BigInt
  override def toString: String = s"DFUInt($width)"
}

object DFUInt {
  def dfType[W](width : TwoFace.Int[W]) = new DFUInt(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  case class Token[W](dfType : DFUInt[W], value : BigInt, bubbleMask : XBitVector[W]) extends DFAny.Token[DFUInt[W]]
}
