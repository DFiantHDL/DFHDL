package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

class DFBits[W] private (val width : TwoFace.Int[W]) extends DFType {
  type Width = W
  type TToken = DFBits.Token[W]
  type `Op==Builder`[-L, -R] = DFBits.`Op==`.Builder[L, R]
  override def toString: String = s"DFBits($width)"
}

object DFBits extends DFAny.Companion {
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

  object `Op==` extends `Op==` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type TType = DFBool}
    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFAny.Of[DFBits[LW]], DFAny.Of[DFBits[RW]]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.dfType(), left, DiSoOp.==, right)
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFAny.Of[DFBits[LW]], DFAny.Of[DFBits[RW]]] =
        create[DFAny.Of[DFBits[LW]], LW, DFAny.Of[DFBits[RW]], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })
    }
  }

//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Comparison operations
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBool.Token]) {
//    object Builder {
//
//      implicit def evDFBits_op_DFBits[LW, RW](
//        implicit
//        ctx : DFAny.Op.Context,
//        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
//      ) : Builder[DFBits[LW], DFBits[RW]] = create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
//        checkLWvRW.unsafeCheck(left.width, right.width)
//        (left, right)
//      })
//
//      implicit def evDFBits_op_Const[LW, R, RW](
//        implicit
//        ctx : DFAny.Op.Context,
//        rConst : Const.Builder.Aux[R, RW],
//      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
//        val right = rConst(rightNum)
//        (left, right)
//      })
//
//      implicit def evConst_op_DFBits[L, LW, RW](
//        implicit
//        ctx : DFAny.Op.Context,
//        lConst : Const.Builder.Aux[L, LW],
//      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
//        val left = lConst(leftNum)
//        (left, right)
//      })
//
//      implicit def evDFBits_op_SBV[LW](
//        implicit
//        ctx : DFAny.Op.Context,
//        rSBV : SameBitsVector.Builder[LW]
//      ) : Builder[DFBits[LW], SameBitsVector] = create[DFBits[LW], LW, SameBitsVector, LW]((left, rightSBV) => {
//        val right = rSBV(left, rightSBV)
//        (left, right)
//      })
//
//      implicit def evSBV_op_DFBits[RW](
//        implicit
//        ctx : DFAny.Op.Context,
//        lSBV : SameBitsVector.Builder[RW]
//      ) : Builder[SameBitsVector, DFBits[RW]] = create[SameBitsVector, RW, DFBits[RW], RW]((leftSBV, right) => {
//        val left = lSBV(right, leftSBV)
//        (left, right)
//      })
//    }
//  }
//  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFBits.Token.==) with `Op==`
//  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFBits.Token.!=) with `Op!=`
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}