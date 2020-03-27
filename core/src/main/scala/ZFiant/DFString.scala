package ZFiant
import java.nio.charset.StandardCharsets

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import ZFiant.compiler.printer.Printer
object DFString {
  final case class Type[L](length : TwoFace.Int[L]) extends DFAny.Type {
    type Length = L
    type Width = Length * 8
    type TToken = Token[Length]
//    type TPattern = DFString.Pattern
//    type TPatternAble[+R] = DFString.Pattern.Able[R]
//    type TPatternBuilder[LType <: DFAny.Type] = DFString.Pattern.Builder[LType]
//    type OpAble[R] = DFString.Op.Able[R]
//    type `Op==Builder`[L0, R] = DFString.`Op==`.Builder[L0, R]
//    type `Op!=Builder`[L0, R] = DFString.`Op!=`.Builder[L0, R]
//    type `Op<>Builder`[LType <: DFAny.Type, R] = DFString.`Op<>`.Builder[LType, R]
//    type `Op:=Builder`[LType <: DFAny.Type, R] = DFString.`Op:=`.Builder[LType, R]
//    type InitAble[L0 <: DFAny] = DFString.Init.Able[L0]
//    type InitBuilder[L0 <: DFAny] = DFString.Init.Builder[L0, TToken]
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](length * 8)
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token = {
      assert(fromToken.width.getValue == width.getValue)
      Token(fromToken.value.toByteArray.toVector, fromToken.isBubble)
    }

    //    override def toString: String = if (logical) "DFString" else "DFBit"
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      s"$TP DFString($LIT$length)"
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[L](checkedLength : Positive.Checked[L])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedLength))
  def apply[L](
    implicit ctx : DFAny.Context, checkedLength : Positive.Checked[L], di: DummyImplicit
  ) = DFAny.NewVar(Type(checkedLength))
  def unapply(arg: DFAny): Option[TwoFace.Int[Int]] = arg.dfType match {
    case Type(length) => Some(length)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token[L](value : Vector[Byte], bubble : Boolean) extends DFAny.Token.Of[Vector[Byte], L * 8] {
    val width: TwoFace.Int[L * 8] = TwoFace.Int.create[L * 8](value.length * 8)
    lazy val valueBits : XBitVector[L * 8] = value.map(b => b.toInt.toBitVector(8) : BitVector).reduce(_ ++ _).asInstanceOf[XBitVector[L * 8]]
    lazy val bubbleMask: XBitVector[L * 8] = bubble.toBitVector(width)
    def +  [RL](that : Token[RL]) : Token[L + RL] = Token(this.value ++ that.value, this.isBubble || that.isBubble)
    def == [RL](that : Token[RL]) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
    def != [RL](that : Token[RL]) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)

    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      val valueStr = new String(value.toArray, StandardCharsets.ISO_8859_1)
      s""""$valueStr""""
    }
  }

  object Token {
    implicit def bubbleOfToken[L] : DFAny.Token.BubbleOfToken[Token[L]] = ??? // t => Token[L](t.length, Bubble)
    implicit def bubbleOfDFType[L] : DFAny.Token.BubbleOfDFType[Type[L]] =  ??? //t => Token[L](t.length, Bubble)
    def apply(value : String) : Token[Int] = Token(value.getBytes(StandardCharsets.ISO_8859_1).toVector, bubble = false)
//    def fromValue(logical : Boolean, value : Boolean) : Token = new Token(logical, value, false)
//    def apply(logical : Boolean, value : Bubble) : Token = new Token(logical, false, true)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
