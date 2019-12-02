package ZFiant

import singleton.twoface._
import DFiant.internals._

class DFBool extends DFType {
  type Width = 1
  type TToken = DFBool.Token
  val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
  override def toString: String = "DFBool()"
}

object DFBool {
  def dfType() = new DFBool()
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(), Seq())

  final case class Token(value : Boolean, bubble : Boolean) extends DFAny.Token.Of[Boolean, 1] {
    val width: TwoFace.Int[1] = 1
    lazy val valueBits : XBitVector[1] = XBitVector.bit(value)
    lazy val bubbleMask: XBitVector[1] = XBitVector.bit(bubble)
    def && (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value && that.value)
    }
    def || (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value || that.value)
    }
    def ^ (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value ^ that.value)
    }
    def unary_! : Token = {
      if (this.isBubble) Token(Bubble)
      else Token(!this.value)
    }
    def select[ST <: DFAny.Token](thenSel : ST, elseSel : ST)(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST]
    ) : ST = {
      if (this.value) if (this.isBubble) bubbleOf(thenSel) else thenSel
      else if (this.isBubble) bubbleOf(elseSel) else elseSel
    }
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = _ => Token(Bubble)
    implicit val bubbleOfDFType : DFAny.Token.BubbleOfDFType[DFBool] = _ => Token(Bubble)
    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)
  }

}


