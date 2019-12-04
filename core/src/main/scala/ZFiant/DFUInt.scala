package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object DFUInt {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token[W]
    override def toString: String = s"DFUInt($width)"
  }
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(width), Seq())

  final case class Token[W](width : TwoFace.Int[W], value : BigInt, bubble : Boolean) extends DFAny.Token.Of[BigInt, W] {
    lazy val valueBits : XBitVector[W] = value.toBitVector(width)
    lazy val bubbleMask: XBitVector[W] = bubble.toBitVector(width)
  }
}
