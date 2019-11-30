package ZFiant

import singleton.twoface._

class DFBool extends DFType {
  type Width = 1
  type TToken = DFBool.Token
  val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
}

object DFBool {
  def dfType() = new DFBool()
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(), Seq())

  case class Token(value : Boolean) extends DFAny.Token
}

