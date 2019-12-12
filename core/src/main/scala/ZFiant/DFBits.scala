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
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFBits.Pattern.Builder[L]
    type OpAble[R] = DFBits.Op.Able[R]
    type `Op==Builder`[-L, -R] = DFBits.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = DFBits.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, -R] = DFBits.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, -R] = DFBits.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFBits.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBits.Init.Builder[L, TToken]
    override def toString: String = s"DFBits($width)"
  }
  def apply[W](width : TwoFace.Int[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(width))

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
//    def << [RW](that : DFUInt.Token[RW]) : Token[W] = {
//      val shift = that.value.toInt
//      val outWidth = this.width
//      val outBitsValue = this.valueBits << shift
//      val outBubbleMask = this.bubbleMask << shift
//      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
//    }
//    def >> [RW](that : DFUInt.Token[RW]) : Token[W] = {
//      val shift = that.value.toInt
//      val outWidth = this.width
//      val outBitsValue = this.valueBits >>> shift
//      val outBubbleMask = this.bubbleMask >>> shift
//      new Token(outWidth, outBitsValue.asInstanceOf[XBitVector[W]], outBubbleMask.asInstanceOf[XBitVector[W]])
//    }
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
//    def toUInt : DFUInt.Token[W] = {
//      val outWidth = this.width
//      val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width)
//      val outBubble = isBubble
//      DFUInt.Token(outWidth, outValueUInt, outBubble)
//    }
//    def toSInt : DFSInt.Token = {
//      val outWidth = this.width
//      val outValueSInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
//      val outBubble = isBubble
//      new DFSInt.Token(outWidth, outValueSInt, outBubble)
//    }

  }
  object Token {
    implicit def bubbleOfToken[W] : DFAny.Token.BubbleOfToken[Token[W]] = t => Token(t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[DFBits.Type[W]] = t => Token[W](t.width, Bubble)
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

    import DFAny.TokenSeq
    def |[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l | r)
    def &[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l & r)
    def ^[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    def concat[W, RW] : (Seq[Token[W]], Seq[Token[RW]]) => Seq[Token[W + RW]] = (left, right) => TokenSeq(left, right)((l, r) => l ## r)
//    def <<[LW, RW] : (Seq[Token[LW]], Seq[DFUInt.Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l << r)
//    def >>[LW, RW] : (Seq[Token[LW]], Seq[DFUInt.Token[RW]]) => Seq[Token[LW]] = (left, right) => TokenSeq(left, right)((l, r) => l >> r)
    def ==[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    def !=[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_~[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => ~t)
    def reverse[W](left : Seq[Token[W]]) : Seq[Token[W]] = TokenSeq(left)(t => t.reverse)
    def resize[LW, RW](left : Seq[Token[LW]], toWidth : TwoFace.Int[RW]) : Seq[Token[RW]] = TokenSeq(left)(t => t.resize(toWidth))
//    def toUInt[W](left : Seq[Token[W]]) : Seq[DFUInt.Token[W]] = TokenSeq(left)(t => t.toUInt)
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
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
        def apply[R](left: DFBits[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.bitVector).foldLeft(Set.empty[BitVector])((set, bitVector) => {
            if (set.contains(bitVector)) throw new IllegalArgumentException(s"\nThe bitvector $bitVector already intersects with $set")
            if (bitVector.length > left.width) throw new IllegalArgumentException(s"\nThe bitvector $bitVector is wider than ${left.meta.name}")
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
        right.toSeqAny.collect{
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
//  implicit def conv[LW, R, RW](rightC : R)(
//    implicit
//    rConst : Const.Builder.Aux[R, RW],
//    checkLWvRW : `Op:=`.Builder.`LW == RW`.CheckedShell[LW, RW]
//  ) : Const[LW] = rConst(rightC).asInstanceOf[Const[LW]]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
//      final def <> [RW](port : DFAny.PortOf[Type[RW]])(
//        implicit op: `Op<>`.Builder[Type[RW], L], ctx : DFNet.Context
//      ) = port.connectWith(op(port.dfType, value))
    }
    class Able2[L](value : L) extends Able[L](value) {
      final val left = value
      final def |  [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      final def &  [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ^  [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
//      final def ## [RW](right : DFBits[RW])(implicit op: `Op##`.Builder[L, DFBits[RW]]) = op(left, right)
    }
    trait Implicits {
      sealed class DFBitsFromBitVector(left : BitVector) extends Able2[BitVector](left)
      final implicit def DFBitsFromBitVector(left: BitVector): DFBitsFromBitVector = new DFBitsFromBitVector(left)
      sealed class DFBitsFromXBitVector[W](left : XBitVector[W]) extends Able2[XBitVector[W]](left)
      final implicit def DFBitsFromXBitVector[W](left: XBitVector[W]): DFBitsFromXBitVector[W] = new DFBitsFromXBitVector[W](left)
      sealed class DFBitsFromZeros(left : SameBitsVector) extends Able2[SameBitsVector](left)
      final implicit def DFBitsFromZeros(left : SameBitsVector) : DFBitsFromZeros = new DFBitsFromZeros(left)
      sealed class DFBitsFromDFBool(left : DFBool)(implicit ctx : DFAny.Context) extends Able2[DFBits[1]](DFAny.Alias.AsIs(Type(1), left))
      final implicit def DFBitsFromDFBool(left: DFBool)(implicit ctx : DFAny.Context): DFBitsFromDFBool = new DFBitsFromDFBool(left)
      final implicit def ofDFBits[W](value : DFBits[W]) : Able[DFBits[W]] = new Able[DFBits[W]](value)
      implicit class Ops[LW](val left : DFBits[LW]){
        final def | [R, RW](right : Able[R])(implicit op: `Op|`.Builder[DFBits[LW], R]) = op(left, right)
      }
//      implicit class PortOps[LW](val port : DFAny.PortOf[Type[LW]]){
//        def <> [R](right : Able[R])(
//          implicit ctx : DFNet.Context, op : `Op<>`.Builder[Type[LW], R]
//        ) : Unit = port.connectWith(op(port.dfType, right))
//      }
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
        checkLWvRW : `LW == RW`.CheckedShellSym[CaseClassSkipper[_], LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] =
        create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[CaseClassSkipper[_], LW, RW]
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `LW == RW`.CheckedShellSym[CaseClassSkipper[_], LW, RW]
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
        val right = rSBV(left.dfType, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW](
        implicit
        ctx : DFAny.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SameBitsVector, DFBits[RW]] = create[SameBitsVector, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right.dfType, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.!=)((l, r) => l != r) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, -R] extends DFAny.Op.Builder[LType, R] {
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
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], DFBits[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFBits_op_SBV[LW](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[Type[LW], SameBitsVector] = (left, right) => {
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

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  protected trait LogicOp[-Op <: DiSoOp] {
    def apply[LW, RW](left : Token[LW], right : Token[RW]) : Token[LW]
  }
  protected val `TokenOp|` : LogicOp[DiSoOp.|] = new LogicOp[DiSoOp.|] {
    def apply[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left | right
  }
  protected val `TokenOp&` : LogicOp[DiSoOp.&] = new LogicOp[DiSoOp.&] {
    def apply[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left & right
  }
  protected val `TokenOp^` : LogicOp[DiSoOp.^] = new LogicOp[DiSoOp.^] {
    def apply[LW, RW](left: Token[LW], right: Token[RW]): Token[LW] = left ^ right
  }
  protected abstract class OpsLogic[Op <: DiSoOp](op : Op)(tokenOp : LogicOp[Op]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[-L, -R, Comp0] = Builder[L, R] {
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
          checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
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
                  DFAny.Func2[Type[LW], DFBits[LW], Op, DFBits[RW]](Type[LW](left.width), left, op, right)((l, r) => tokenOp(l, r))
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
        RequireMsgSym[false, "An unconstrained-width literal cannot be used in a logic operation", Builder[_,_]]

      implicit def evDFBits_op_SBV[LW](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SameBitsVector, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW](implicit error : UnconstrainedLiteralError)
      : Aux[SameBitsVector, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(DiSoOp.|)(`TokenOp|`)
  object `Op&` extends OpsLogic(DiSoOp.&)(`TokenOp&`)
  object `Op^` extends OpsLogic(DiSoOp.^)(`TokenOp^`)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}