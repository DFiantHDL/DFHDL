package ZFiant

import singleton.twoface._
import DFiant.internals._

object DFBool {
  final case class Type() extends DFAny.DFType {
    type Width = 1
    type TToken = Token
    val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
    override def toString: String = "DFBool()"
  }
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(), Seq())

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
    def == (that : Token) : Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    def != (that : Token) : Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = _ => Token(Bubble)
    implicit val bubbleOfDFType : DFAny.Token.BubbleOfDFType[DFBool.Type] = _ => Token(Bubble)
    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)

    import DFAny.TokenSeq
    val || : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l || r)
    val && : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l && r)
    val ^  : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    val == : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    val != : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_! (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => !t)
    def select[ST <: DFAny.Token](cond : Seq[Token], thenSel : Seq[ST], elseSel : Seq[ST])(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST]
    ) : Seq[ST] = TokenSeq(cond, thenSel, elseSel)((c, t, e) => c.select(t, e))
  }

}


