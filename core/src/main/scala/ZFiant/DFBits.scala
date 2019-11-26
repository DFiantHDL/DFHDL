package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFBits[W], false] {
  type Width = W
  type TToken = DFBits.Token
}

object DFBits {
  def mold[W](width : TwoFace.Int[W]) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(new DFBits(width))(Seq())

  case class Token(value : Int) extends DFAny.Token
}