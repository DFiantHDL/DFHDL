package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFBits.Token
}

object DFBits {
  def dfType[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  case class Token(value : Int) extends DFAny.Token
}