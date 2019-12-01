package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

class DFUInt[W] private (val width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFUInt.Token[W]
  override def toString: String = s"DFUInt($width)"
}

object DFUInt {
  def dfType[W](width : TwoFace.Int[W]) = new DFUInt(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  final case class Token[W](width : TwoFace.Int[W], value : BigInt, bubble : Boolean) extends DFAny.Token.Of[BigInt, W] {
    lazy val valueBits : XBitVector[W] = value.toBitVector(width)
    lazy val bubbleMask: XBitVector[W] = bubble.toBitVector(width)
  }
}
