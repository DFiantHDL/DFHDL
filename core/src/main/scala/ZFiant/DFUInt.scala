package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFUInt[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFUInt[W], false] {
  type Width = W
  type TToken = DFUInt.Token
}

object DFUInt {
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(new DFUInt(width))(Seq())

  case class Token(value : Int) extends DFAny.Token
}
