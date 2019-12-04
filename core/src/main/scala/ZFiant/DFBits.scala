package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object b0s extends DFBits.SameBitsVector(false)
object b1s extends DFBits.SameBitsVector(true)

object DFBits extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token[W]
    type `Op==Builder`[-L, -R] = `Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = `Op!=`.Builder[L, R]
    override def toString: String = s"DFBits($width)"
  }
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(width), Seq())

  final case class Token[W](width : TwoFace.Int[W], value : XBitVector[W], bubbleMask : XBitVector[W]) extends DFAny.Token.Of[XBitVector[W], W] {
    lazy val valueBits : XBitVector[W] = value
    def |[RW] (that : Token[RW]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits | that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def &[RW] (that : Token[RW]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits & that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def ^[RW] (that : Token[RW]) : Token[W] = {
      assert(that.width == width)
      val outBitsValue = this.valueBits ^ that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      Token(width, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def ## [RW](that : Token[RW]) : Token[W + RW] = {
      val outWidth = this.width + that.width
      val outBitsValue = this.valueBits ++ that.valueBits
      val outBubbleMask = this.bubbleMask ++ that.bubbleMask
      Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W + RW]], outBubbleMask.asInstanceOf[XBitVector[W + RW]])
    }
    def << [RW](that : DFUInt.Token[RW]) : Token[W] = {
      val shift = that.value.toInt
      val outWidth = this.width
      val outBitsValue = this.valueBits << shift
      val outBubbleMask = this.bubbleMask << shift
      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
    }
    def >> [RW](that : DFUInt.Token[RW]) : Token[W] = {
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
    def resize[RW](toWidth : TwoFace.Int[RW]) : Token[RW] = {
      if (toWidth < width) bitsWL(toWidth, 0)
      else if (toWidth > width) (Token(toWidth - width, 0) ## this).asInstanceOf[Token[RW]]
      else this.asInstanceOf[Token[RW]]
    }
    def == [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.valueBits == that.valueBits, this.isBubble || that.isBubble)
    def != [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.valueBits != that.valueBits, this.isBubble || that.isBubble)
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
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[DFBits.Type[W]] = t => Token(t.width, Bubble)
    def apply[W](width : TwoFace.Int[W], value : Int) : Token[W] = Token(width, BigInt(value).toBitVector(width))
    def apply[W](width : TwoFace.Int[W], value : BitVector) : Token[W] = {
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
    def |[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l | r)
    def &[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l & r)
    def ^[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    def concat[W, RW] : (Seq[Token[W]], Seq[Token[RW]]) => Seq[Token[W + RW]] = (left, right) => TokenSeq(left, right)((l, r) => l ## r)
    def <<[LW, RW] : (Seq[Token[LW]], Seq[DFUInt.Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l << r)
    def >>[LW, RW] : (Seq[Token[LW]], Seq[DFUInt.Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l >> r)
    def ==[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    def !=[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_~[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => ~t)
    def reverse[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => t.reverse)
    def resize[LW, RW](left : Seq[Token[LW]], toWidth : TwoFace.Int[RW]) : Seq[Token[RW]] = TokenSeq(left)(t => t.resize(toWidth))
    def toUInt[W](left : Seq[Token[W]]) : Seq[DFUInt.Token[W]] = TokenSeq(left)(t => t.toUInt)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : DFBits[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromBitVector(implicit ctx : DFAny.Context)
      : Aux[BitVector, Int] = new Builder[BitVector] {
        type W = Int
        def apply(value : BitVector) : DFBits[W] = {
          val width = TwoFace.Int(value.length.toInt)
          DFAny.Const[Type[Int]](Type(width), Token(width, value))
        }
      }
      implicit def fromXBitVector[W0](implicit ctx : DFAny.Context)
      : Aux[XBitVector[W0], W0] = new Builder[XBitVector[W0]] {
        type W = W0
        def apply(value : XBitVector[W0]) : DFBits[W] = {
          val width = TwoFace.Int.create[W0](value.length.toInt)
          DFAny.Const[Type[W0]](Type(width), Token(width, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[ZFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[W] {
      def apply(bits : DFBits[W], sbv : SameBitsVector) : DFBits[W]
    }
    object Builder {
      implicit def ev[W](implicit ctx : DFAny.Context)
      : Builder[W] = (bits, sbv) => DFAny.Const[Type[W]](Type(bits.width), Token(XBitVector.fill(bits.width)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: DiSoOp](op : Op)(func : (Token[_], Token[_]) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}
    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFBits[LW], DFBits[RW]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(), left, op, right)(func)
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] =
        create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_SBV[LW](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[DFBits[LW], SameBitsVector] = create[DFBits[LW], LW, SameBitsVector, LW]((left, rightSBV) => {
        val right = rSBV(left, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW](
        implicit
        ctx : DFAny.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SameBitsVector, DFBits[RW]] = create[SameBitsVector, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.!=)((l, r) => l != r) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}