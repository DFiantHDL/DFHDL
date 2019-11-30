package ZFiant

import singleton.ops._
import singleton.twoface._

case class DFUInt[W] private (width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFUInt.Token
}

object DFUInt {
  def dfType[W](width : TwoFace.Int[W]) = new DFUInt(width)
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(dfType(width), Seq())

  case class Token(value : Int) extends DFAny.Token
}
