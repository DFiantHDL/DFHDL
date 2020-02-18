package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.Func2
object b0s extends DFBits.SameBitsVector(false)
object b1s extends DFBits.SameBitsVector(true)

object DFBits extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token[W]
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBits.Pattern.Builder[LType]
    type OpAble[R] = DFBits.Op.Able[R]
    type `Op==Builder`[L, R] = DFBits.`Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = DFBits.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = DFBits.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = DFBits.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFBits.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBits.Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token = fromToken
    override def toString: String = s"DFBits[$width]"
    def codeString : String = s"DFBits($width)"
  }
  def apply[W](checkedWidth : BitsWidth.Checked[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedWidth))
  def apply[W](
    implicit ctx : DFAny.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) = DFAny.NewVar(Type(checkedWidth))

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
    def toSInt : DFSInt.Token[W] = {
      val outWidth = this.width
      val outValueSInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
      val outBubble = isBubble
      new DFSInt.Token(outWidth, outValueSInt, outBubble)
    }
    def codeString : String = value.codeString
  }
  object Token {
    implicit def bubbleOfToken[W] : DFAny.Token.BubbleOfToken[Token[W]] = t => Token(t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => Token[W](t.width, Bubble)
    def apply[W](width : TwoFace.Int[W], value : Int) : Token[W] = Token[W](width, BigInt(value).toBitVector(width))
    def apply[W](width : TwoFace.Int[W], value : BitVector) : Token[W] = {
      assert(value.length == width.getValue, s"\nThe init vector $value must have a width of $width")
      Token(width, value.toLength(width), XBitVector.low(width))
    }
    def apply[W](value : XBitVector[W]) : Token[W] = Token[W](TwoFace.Int.create[W](value.length.toInt), value)
    def apply[W](width : TwoFace.Int[W], value : Bubble) : Token[W] = Token[W](width, XBitVector.low(width), XBitVector.high(width))
    def apply[W](width : TwoFace.Int[W], value : Token[W]) : Token[W] = {
      assert(value.width == width, s"\nThe init vector $value must have a width of $width")
      value.bitsWL(width, 0)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[BitVector]) extends DFAny.Pattern.OfSet[BitVector, Pattern](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val bitVector : BitVector
    }
    object Able {
      implicit class DFUIntPatternBitVector[R <: BitVector](val right : R) extends Able[R] {
        val bitVector : BitVector = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.bitVector).foldLeft(Set.empty[BitVector])((set, bitVector) => {
            if (set.contains(bitVector)) throw new IllegalArgumentException(s"\nThe bitvector $bitVector already intersects with $set")
            if (bitVector.length > left.width) throw new IllegalArgumentException(s"\nThe bitvector $bitVector is wider than ${left.width}")
            set + bitVector
          })

          new Pattern(patternSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
      implicit class DFBitsSameBitsVector[LW](val right : SameBitsVector) extends Able[DFBits[LW]]
      implicit class DFBitsToken[LW](val right : Token[LW]) extends Able[DFBits[LW]]
      implicit class DFBitsTokenSeq[LW](val right : Seq[Token[LW]]) extends Able[DFBits[LW]]
      implicit class DFBitsBitVector[LW](val right : BitVector) extends Able[DFBits[LW]]
      implicit class DFBitsSeqOfBitVector[LW](val right : Seq[BitVector]) extends Able[DFBits[LW]]
      implicit class DFBitsXBitVector[LW](val right : XBitVector[LW]) extends Able[DFBits[LW]]

      def toTokenSeq[LW](width : TwoFace.Int[LW], right : Seq[Able[DFBits[LW]]]) : Seq[Token[LW]] =
        right.toSeqAny.collect {
          case t : Bubble => Token(width, t)
          case t : Token[_] => t.asInstanceOf[Token[LW]]
          case t : BitVector => Token(width, t)
          case t : SameBitsVector => Token(width, XBitVector.fill(width)(t.value))
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW], Token[LW]] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type Const[W] = DFAny.Const[Type[W]]
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : Const[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromBitVector(implicit ctx : DFAny.Context)
      : Aux[BitVector, Int] = new Builder[BitVector] {
        type W = Int
        def apply(value : BitVector) : Const[W] = {
          val width = TwoFace.Int(value.length.toInt)
          DFAny.Const[Type[Int]](Type(width), Token(width, value))
        }
      }
      implicit def fromXBitVector[W0](implicit ctx : DFAny.Context)
      : Aux[XBitVector[W0], W0] = new Builder[XBitVector[W0]] {
        type W = W0
        def apply(value : XBitVector[W0]) : Const[W] = {
          val width = TwoFace.Int.create[W0](value.length.toInt)
          DFAny.Const[Type[W0]](Type(width), Token(width, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      final val left = value
      final def |   [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      final def &   [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ^   [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      final def === [RW](right : DFBits[RW])(implicit op: `Op===`.Builder[L, DFBits[RW]]) = op(left, right)
      final def =!= [RW](right : DFBits[RW])(implicit op: `Op=!=`.Builder[L, DFBits[RW]]) = op(left, right)
//      final def ## [RW](right : DFBits[RW])(implicit op: `Op##`.Builder[L, DFBits[RW]]) = op(left, right)
    }
    trait Implicits {
      sealed class DFBitsFromBitVector(left : BitVector) extends AbleOps[BitVector](left)
      final implicit def DFBitsFromBitVector(left: BitVector): DFBitsFromBitVector = new DFBitsFromBitVector(left)
      sealed class DFBitsFromXBitVector[W](left : XBitVector[W]) extends AbleOps[XBitVector[W]](left)
      final implicit def DFBitsFromXBitVector[W](left: XBitVector[W]): DFBitsFromXBitVector[W] = new DFBitsFromXBitVector[W](left)
      sealed class DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) extends AbleOps[SBV](left)
      final implicit def DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : DFBitsFromZeros[SBV] = new DFBitsFromZeros(left)
//      sealed class DFBitsFromDFBool(left : DFBool)(implicit ctx : DFAny.Context) extends AbleOps[DFBits[1]](DFAny.Alias.AsIs(Type(1), left))
//      final implicit def DFBitsFromDFBool(left: DFBool)(implicit ctx : DFAny.Context): DFBitsFromDFBool = new DFBitsFromDFBool(left)
      sealed class DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) extends AbleOps[DFBits[W]](left)
      final implicit def DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) : DFBitsFromDefaultRet[W] = new DFBitsFromDefaultRet(left)
      final implicit def ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = new Able(left)
      final implicit class DFBitsOps[LW](val left : DFBits[LW]){
        def |   [R](right : Able[R])(implicit op: `Op|`.Builder[DFBits[LW], R]) = op(left, right)
        def &   [R](right : Able[R])(implicit op: `Op&`.Builder[DFBits[LW], R]) = op(left, right)
        def ^   [R](right : Able[R])(implicit op: `Op^`.Builder[DFBits[LW], R]) = op(left, right)
        def === [R](right : Able[R])(implicit op: `Op===`.Builder[DFBits[LW], R]) = op(left, right)
        def =!= [R](right : Able[R])(implicit op: `Op=!=`.Builder[DFBits[LW], R]) = op(left, right)
        def unary_~(implicit ctx : DFAny.Context) : DFBits[LW] = DFAny.Alias.Invert(left)
        def << [R](right: DFUInt.Op.Able[R])(implicit op: `Op<<`.Builder[DFBits[LW], R]) = op(left, right)
        def >> [R](right: DFUInt.Op.Able[R])(implicit op: `Op>>`.Builder[DFBits[LW], R]) = op(left, right)
        def resize[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) =
          DFAny.Alias.Resize.bits(left, toWidth)
      }
      final implicit class DFBitsAliases[LW, Mod <: DFAny.Modifier](val left : DFAny.Value[Type[LW], Mod]) {
        def uint(implicit ctx : DFAny.Context) : DFAny.Value[DFUInt.Type[LW], Mod] =
          left.as(DFUInt.Type(left.width)).overrideCodeString(rs => s"$rs.uint")
        def sint(implicit ctx : DFAny.Context) : DFAny.Value[DFSInt.Type[LW], Mod] =
          left.as(DFSInt.Type(left.width)).overrideCodeString(rs => s"$rs.sint")
        def apply[H, L](relBitHigh : BitIndex.Checked[H, left.Width], relBitLow : BitIndex.Checked[L, left.Width])(
          implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
        ) : DFAny.Value[DFBits.Type[relWidth.Out], Mod] = left.bits(relBitHigh, relBitLow)
      }
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[ZFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[W] {
      def apply(bits : Type[W], sbv : SameBitsVector) : Const[W]
    }
    object Builder {
      implicit def ev[W](implicit ctx : DFAny.Context)
      : Builder[W] = (bits, sbv) => DFAny.Const[Type[W]](Type(bits.width), Token(XBitVector.fill(bits.width)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, R] extends DFAny.Op.Builder[LType, R] {
      type Out = DFAny.Of[LType]
    }

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "An assignment/connection operation does not permit different widths. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFBits[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[Type[LW], SBV] = (left, right) => {
        rSBV(left, right)
      }

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token[_], Token[_]) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}
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
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] =
        create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[DFBits[LW], SBV] = create[DFBits[LW], LW, SBV, LW]((left, rightSBV) => {
        val right = rSBV(left.dfType, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SBV, DFBits[RW]] = create[SBV, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right.dfType, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(Func2.Op.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)((l, r) => l == r)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsLogic[Op <: Func2.Op](op : Op) {
    def tokenOp[LW, RW](left : Token[LW], right : Token[RW]) : Token[LW]
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Logic operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW](
          implicit
          ctx : DFAny.Context,
          checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[LW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[LW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[LW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  DFAny.Func2[Type[LW], DFBits[LW], Op, DFBits[RW]](Type[LW](left.width), left, op, right)(tokenOp)
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsg[false, "An unconstrained-width literal cannot be used in a logic operation"]

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SBV, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[SBV, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(Func2.Op.|) {
    def tokenOp[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left | right
  }
  object `Op&` extends OpsLogic(Func2.Op.&) {
    def tokenOp[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left & right
  }
  object `Op^` extends OpsLogic(Func2.Op.^) {
    def tokenOp[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left ^ right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op<<` extends OpsShift[Type](Func2.Op.<<) {
    def tokenFunc[LW, RW](left: DFBits.Token[LW], right: DFUInt.Token[RW]) : DFBits.Token[LW] = left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW, RW](left: DFBits.Token[LW], right: DFUInt.Token[RW]) : DFBits.Token[LW] = left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}