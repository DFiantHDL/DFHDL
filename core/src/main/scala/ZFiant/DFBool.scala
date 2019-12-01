package ZFiant

import singleton.twoface._

class DFBool extends DFType {
  type Width = 1
  type TTokenValue = Boolean
  val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
  override def toString: String = "DFBool"
}

object DFBool {
  def dfType() = new DFBool()
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(), Seq())

  case class Token(dfType : DFBool, value : Boolean, bubbleMask : XBitVector[1]) extends DFAny.Token[DFBool]
}

