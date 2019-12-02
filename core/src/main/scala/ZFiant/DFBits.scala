package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFBits.Token[W]
  override def toString: String = s"DFBits($width)"
}

object DFBits {
  def dfType[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(dfType(width), Seq())

  final case class Token[W](width : TwoFace.Int[W], value : XBitVector[W], bubbleMask : XBitVector[W]) extends DFAny.Token.Of[XBitVector[W], W] {
    lazy val valueBits : XBitVector[W] = value
    def | (that : Token[W]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits | that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def & (that : Token[W]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits & that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def ^ (that : Token[W]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits ^ that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def ## [W2](that : Token[W2]) : Token[W + W2] = {
      val outWidth = this.width + that.width
      val outBitsValue = this.valueBits ++ that.valueBits
      val outBubbleMask = this.bubbleMask ++ that.bubbleMask
      Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W + W2]], outBubbleMask.asInstanceOf[XBitVector[W + W2]])
    }
    def << [W2](that : DFUInt.Token[W2]) : Token[W] = {
      val shift = that.value.toInt
      val outWidth = this.width
      val outBitsValue = this.valueBits << shift
      val outBubbleMask = this.bubbleMask << shift
      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def >> [W2](that : DFUInt.Token[W2]) : Token[W] = {
      val shift = that.value.toInt
      val outWidth = this.width
      val outBitsValue = this.valueBits >>> shift
      val outBubbleMask = this.bubbleMask >>> shift
      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def unary_~ : Token[W] = {
      val outWidth = this.width
      val outBitsValue = ~this.valueBits
      val outBubbleMask = this.bubbleMask
      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask)
    }
    def reverse : Token[W] = {
      val outWidth = this.width
      val outBitsValue = this.valueBits.reverseBitOrder.asInstanceOf[XBitVector[W]]
      val outBubbleMask = this.bubbleMask.reverseBitOrder.asInstanceOf[XBitVector[W]]
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    def resize[W2](toWidth : TwoFace.Int[W2]) : Token[W2] = {
      if (toWidth < width) bitsWL(toWidth, 0)
      else if (toWidth > width) (Token(toWidth - width, 0) ## this).asInstanceOf[Token[W2]]
      else this.asInstanceOf[Token[W2]]
    }
    def == (that : Token[W]) : DFBool.Token = DFBool.Token(this.valueBits == that.valueBits, this.isBubble || that.isBubble)
    def != (that : Token[W]) : DFBool.Token = DFBool.Token(this.valueBits != that.valueBits, this.isBubble || that.isBubble)
    def toUInt : DFUInt.Token[W] = {
      val outWidth = this.width
      val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width)
      val outBubble = isBubble
      DFUInt.Token(outWidth, outValueUInt, outBubble)
    }
//    def toSInt : DFSInt.Token = {
//      val outWidth = this.width
//      val outValueSInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
//      val outBubble = isBubble
//      new DFSInt.Token(outWidth, outValueSInt, outBubble)
//    }

  }
  object Token {
    implicit def bubbleOfToken[W] : DFAny.Token.BubbleOfToken[Token[W]] = t => Token(t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[DFBits[W]] = t => Token(t.width, Bubble)
    def apply[W](width : TwoFace.Int[W], value : Int) : Token[W] = Token(width, BigInt(value).toBitVector(width))
    def apply[W](width : TwoFace.Int[W], value : XBitVector[W]) : Token[W] = {
      assert(value.length == width.getValue, s"\nThe init vector $value must have a width of $width")
      Token(width, value.toLength(width), XBitVector.low(width))
    }
    def apply[W](value : XBitVector[W]) : Token[W] = Token(TwoFace.Int.create[W](value.length.toInt), value)
    def apply[W](width : TwoFace.Int[W], value : Bubble) : Token[W] = Token(width, XBitVector.low(width), XBitVector.high(width))
    def apply[W](width : TwoFace.Int[W], value : Token[W]) : Token[W] = {
      assert(value.width == width, s"\nThe init vector $value must have a width of $width")
      value.bitsWL(width, 0)
    }

    import DFAny.TokenSeq
    def |[W] : (Seq[Token[W]], Seq[Token[W]]) => Seq[Token[W]] = (left, right) => TokenSeq(left, right)((l, r) => l | r)
    def &[W] : (Seq[Token[W]], Seq[Token[W]]) => Seq[Token[W]] = (left, right) => TokenSeq(left, right)((l, r) => l & r)
    def ^[W] : (Seq[Token[W]], Seq[Token[W]]) => Seq[Token[W]] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    def concat[W, W2] : (Seq[Token[W]], Seq[Token[W2]]) => Seq[Token[W + W2]] = (left, right) => TokenSeq(left, right)((l, r) => l ## r)
    def <<[W, W2] : (Seq[Token[W]], Seq[DFUInt.Token[W2]]) => Seq[Token[W]] = (left, right) => TokenSeq(left, right)((l, r) => l << r)
    def >>[W, W2] : (Seq[Token[W]], Seq[DFUInt.Token[W2]]) => Seq[Token[W]] = (left, right) => TokenSeq(left, right)((l, r) => l >> r)
    def ==[W] : (Seq[Token[W]], Seq[Token[W]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    def !=[W] : (Seq[Token[W]], Seq[Token[W]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_~[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => ~t)
    def reverse[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => t.reverse)
    def resize[W, W2](left : Seq[Token[W]], toWidth : TwoFace.Int[W2]) : Seq[Token[W2]] = TokenSeq(left)(t => t.resize(toWidth))
    def toUInt[W](left : Seq[Token[W]]) : Seq[DFUInt.Token[W]] = TokenSeq(left)(t => t.toUInt)
  }
}