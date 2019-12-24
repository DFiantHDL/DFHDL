package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object DFUInt extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token[W]
    type TPattern = DFUInt.Pattern
    type TPatternAble[+R] = DFUInt.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFUInt.Pattern.Builder[LType]
    type OpAble[R] = DFUInt.Op.Able[R]
    type `Op==Builder`[-L, -R] = DFUInt.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = DFUInt.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, -R] = DFUInt.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, -R] = DFUInt.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFUInt.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFUInt.Init.Builder[L, TToken]
    override def toString: String = s"DFUInt[$width]"
    def codeString(implicit getset : MemberGetSet) : String = s"DFUInt($width)"
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : BitsWidth.Checked[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedWidth))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Token[W](width : TwoFace.Int[W], value : BigInt, bubble : Boolean) extends DFAny.Token.Of[BigInt, W] { //with DFAny.Token.Resizable
    lazy val valueBits : XBitVector[W] = value.toBitVector(width)
    lazy val bubbleMask: XBitVector[W] = bubble.toBitVector(width)
//    def mkTokenU[RW](that : Token[RW], result : BigInt, resultWidth : Int) : Token = {
//      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
//      else Token(resultWidth, result.asUnsigned(resultWidth))
//    }
//
//    final def + [RW](that : Token[RW]) : Token = mkTokenU(that, this.value + that.value, scala.math.max(this.width, that.width) + 1)
//    final def - [RW](that : Token[RW]) : Token = mkTokenU(that, this.value - that.value, scala.math.max(this.width, that.width) + 1)
//    final def * [RW](that : Token[RW]) : Token = mkTokenU(that, this.value * that.value, this.width + that.width)
//    final def / [RW](that : Token[RW]) : Token = mkTokenU(that, this.value / that.value, this.width)
//    final def % [RW](that : Token[RW]) : Token = mkTokenU(that, this.value % that.value, that.width)
    final def <  [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value < that.value, this.isBubble || that.isBubble)
    final def >  [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value > that.value, this.isBubble || that.isBubble)
    final def <= [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value <= that.value, this.isBubble || that.isBubble)
    final def >= [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value >= that.value, this.isBubble || that.isBubble)
    final def == [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    final def != [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
//    def resize(toWidth : Int) : Token = bits.resize(toWidth).toUInt
    def codeString : String = value.codeString
  }

  object Token {
    implicit def bubbleOfToken[W] : DFAny.Token.BubbleOfToken[Token[W]] = t => Token[W](t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => Token[W](t.width, Bubble)

    def apply[W](width : TwoFace.Int[W], value : Int) : Token[W] = Token(width, BigInt(value))
    def apply[W](width : TwoFace.Int[W], value : Long) : Token[W] = Token(width, BigInt(value))
    def apply[W](width : TwoFace.Int[W], value : BigInt) : Token[W] = {
      if (value < 0) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      assert(value.bitsWidth <= width, s"\nThe init value $value width must smaller or equal to $width")
      new Token(width, value, false)
    }
    def apply[W](width : TwoFace.Int[W], value : Bubble) : Token[W] = new Token(width, 0, true)
    def apply[W](width : TwoFace.Int[W], token : Token[W]) : Token[W] = {
      assert(token.width <= width, s"\nThe init value $token width must smaller or equal to $width")
      new Token(width, token.value, token.bubble)
    }

    import DFAny.TokenSeq
//    val + : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l + r)
//    val - : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l - r)
//    val * : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l * r)
//    val / : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l / r)
//    val % : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l % r)
    def < [LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l <  r)
    def > [LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l >  r)
    def <=[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l <= r)
    def >=[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l >= r)
    def ==[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    def !=[LW, RW] : (Seq[Token[LW]], Seq[Token[RW]]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
//    def resize(left : Seq[Token], toWidth : Int) : Seq[Token] = TokenSeq(left)(t => t.resize(toWidth))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(intervalSet : IntervalSet[BigInt]) extends DFAny.Pattern.OfIntervalSet[BigInt, Pattern](intervalSet)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val interval : Interval[BigInt]
    }
    object Able {
      implicit class DFUIntPatternInt[R <: Int](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFULongPatternLong[R <: Long](val right : R)(implicit di : DummyImplicit) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFUIntPatternBigInt[R <: BigInt](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(right)
      }
      implicit class DFUIntPatternRange[R <: Range](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.fromRange(right).toBigIntInterval
      }
      implicit class DFUIntPatternIntervalInt[R <: Interval[Int]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFUIntPatternIntervalLong[R <: Interval[Long]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFUIntPatternIntervalBigInt[R <: Interval[BigInt]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val reqInterval = IntervalSet(Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(left.width)))
          val patternSet = right.map(e => e.interval).foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
            if (set.intersect(interval).nonEmpty) throw new IllegalArgumentException(s"\nThe interval $interval already intersects with $set")
            if (!reqInterval.contains(interval)) throw new IllegalArgumentException(s"\nThe interval $interval is outside of range allowed: $reqInterval")
            set + interval
          })

          require(patternSet.intersect(reqInterval).nonEmpty, s"\nPattern must intersect with $reqInterval. Pattern is: $patternSet")
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
      private type IntWithinWidth[LW] = CompileTime[Natural.Int.Cond[GetArg0] && (BitsWidthOf.CalcInt[GetArg0] <= LW)]
      private type LongWithinWidth[LW] = CompileTime[Natural.Long.Cond[GetArg0] && (BitsWidthOf.CalcLong[GetArg0] <= LW)]
      implicit class DFUIntBubble[LW](val right : Bubble) extends Able[DFUInt[LW]]
      implicit class DFUIntToken[LW](val right : DFUInt.Token[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntTokenSeq[LW](val right : Seq[DFUInt.Token[LW]]) extends Able[DFUInt[LW]]
      implicit class DFUIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntBigInt[LW](val right : BigInt) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfInt[LW](val right : Seq[Int]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfLong[LW](val right : Seq[Long]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfBigInt[LW](val right : Seq[BigInt]) extends Able[DFUInt[LW]]

      def toTokenSeq[LW](width : TwoFace.Int[LW], right : Seq[Able[DFUInt[LW]]]) : Seq[Token[LW]] =
        right.toSeqAny.collect {
          case (t : Bubble) => DFUInt.Token(width, t)
          case (t : Token[_]) => t.asInstanceOf[Token[LW]]
          case (t : Int) => DFUInt.Token(width, t)
          case (t : Long) => DFUInt.Token(width, t)
          case (t : BigInt) => DFUInt.Token(width, t)
        }

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFUInt[LW], Token[LW]] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait PosNeg[N] {
      type W
      def apply(value : N) : (DFUInt[W], Boolean)
    }
    object PosNeg {
      type Aux[N, W0] = PosNeg[N]{type W = W0}
      import singleton.ops.math.Abs
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Context, w : BitsWidthOf.Int[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          val width = w(absValue)
          (DFAny.Const[Type[W]](Type(width), Token(width, absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Context, w : BitsWidthOf.Long[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          val width = w(absValue)
          (DFAny.Const[Type[W]](Type(width), Token(width, absValue)), value < 0)
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Context)
      : Aux[N, Int] = new PosNeg[N] {
        type W = Int
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = value.abs
          val width = TwoFace.Int(absValue.bitsWidth)
          (DFAny.Const[Type[W]](Type(width), Token(width, absValue)), value < 0)
        }
      }
    }
    trait PosOnly[Sym, N] {
      type W
      def apply(value : N) : DFUInt[W]
    }
    object PosOnly {
      type Aux[Sym, N, W0] = PosOnly[Sym, N]{type W = W0}
      object `N >= 0` extends `N >= 0` {
        type MsgCommon[N] = "Operation or assignment do not permit a negative number. Found literal: " + ToString[N]
      }
      implicit def fromInt[Sym, N <: Int](
        implicit
        ctx : DFAny.Context,
        checkPos : `N >= 0`.Int.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Int[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromLong[Sym, N <: Long](
        implicit
        ctx : DFAny.Context,
        checkPos : `N >= 0`.Long.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Long[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromBigInt[Sym, N <: BigInt](implicit ctx : DFAny.Context)
      : Aux[Sym, N, Int] = new PosOnly[Sym, N] {
        type W = Int
        def apply(value : N) : DFUInt[W] = {
          `N >= 0`.BigInt.unsafeCheck(value)
          val width = TwoFace.Int(value.bitsWidth)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
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
      val left = value
//      def +  [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, Extendable[Int], DFUInt[RW]]) = op(left, right)
//      def -  [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, Extendable[Int], DFUInt[RW]]) = op(left, right)
      final def <  [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def >  [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def <= [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def >= [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
    }
    trait Implicits {
      sealed class DFUIntFromInt[L <: Int](left : L) extends AbleOps[L](left)
      final implicit def DFUIntFromInt[L <: Int](left: L): DFUIntFromInt[L] = new DFUIntFromInt(left)
      sealed class DFUIntFromXInt[L <: XInt](left : L) extends AbleOps[L](left)
      final implicit def DFUIntFromXInt[L <: XInt](left: L): DFUIntFromXInt[L] = new DFUIntFromXInt(left)
      sealed class DFUIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def DFUIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFUIntFromLong[L] = new DFUIntFromLong(left)
      sealed class DFUIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def DFUIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFUIntFromXLong[L] = new DFUIntFromXLong(left)
      sealed class DFUIntFromBigInt[L <: BigInt](left : L) extends AbleOps[L](left)
      final implicit def DFUIntFromBigInt[L <: BigInt](left: L): DFUIntFromBigInt[L] = new DFUIntFromBigInt[L](left)
      sealed class DFUIntFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]]) extends AbleOps[DFUInt[W]](left)
      final implicit def DFUIntFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]]) : DFUIntFromDefaultRet[W] = new DFUIntFromDefaultRet(left)
      final implicit def ofDFUInt[W](left : DFUInt[W]) : Able[DFUInt[W]] = new Able(left)
      implicit class DFUIntOps[LW](val left : DFUInt[LW]){
        final def <  [R, RW](right : Able[R])(implicit op: `Op<`.Builder[DFUInt[LW], R]) = op(left, right)
        final def >  [R, RW](right : Able[R])(implicit op: `Op>`.Builder[DFUInt[LW], R]) = op(left, right)
        final def <= [R, RW](right : Able[R])(implicit op: `Op<=`.Builder[DFUInt[LW], R]) = op(left, right)
        final def >= [R, RW](right : Able[R])(implicit op: `Op>=`.Builder[DFUInt[LW], R]) = op(left, right)
      }
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>`[SkipLengthCheck] extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, -R] extends DFAny.Op.Builder[LType, R] {
      type Out = DFAny.Of[LType]
    }

    object Builder {
      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = SkipLengthCheck || (LW >= RW)
        type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFUInt_op_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], DFUInt[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFUInt_op_Const[LW, R, RW](
        implicit
        ctx :  DFAny.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`[false]
  object `Op<>` extends `Ops:=,<>`[true]
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

      object `VecW >= ConstW` extends Checked1Param.Int { //Needs to be mitigated to a warning
        type Cond[VW, CW] = VW >= CW
        type Msg[VW, CW] = "A static boolean result detected, due to an unsigned comparison between a DF variable and a larger number. Found: DFVar-width = "+ ToString[VW] + " and Num-width = " + ToString[CW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(), left, op, right)(func)
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFUInt[LW], DFUInt[RW]] = create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_], L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.==)(_ == _) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.!=)(_ != _) with `Op!=`
  object `Op<`  extends OpsCompare(DiSoOp.< )(_ <  _)
  object `Op>`  extends OpsCompare(DiSoOp.> )(_ >  _)
  object `Op<=` extends OpsCompare(DiSoOp.<=)(_ <= _)
  object `Op>=` extends OpsCompare(DiSoOp.>=)(_ >= _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // +/- operation
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) {
//    //NCW = No-carry width
//    //WCW = With-carry width
//    final class Component[NCW, WCW](_wc : DFFunc2[_,_,_] with DFUInt[WCW])(implicit ctx : DFAny.Context) extends
//      DFAny.Alias[DFUInt[NCW]](DFAny.Alias.Reference.Resize(_wc, _wc.width-1)) with DFUInt[NCW] with CompAlias { //,if (wc.isFolded) "" else s".bits(${wc.width-2}, 0).uint"
//      lazy val wc = {_wc.usedAsWide = true; _wc}
//      lazy val c = new DFBool.Alias(DFAny.Alias.Reference.BitsWL(wc, 1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx}C")
//      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
//      lazy val comp = wc
//      //      lazy val bypassAlias = c.isNotDiscovered
//    }
//
//    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
//    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]
//
//    object Builder {
//      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
//        type Out = Comp0
//      }
//
//      object `LW >= RW` extends Checked1Param.Int {
//        type Cond[LW, RW] = LW >= RW
//        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
//        type ParamFace = Int
//        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
//      }
//
//      object Inference {
//        import singleton.ops.math.Max
//        type CalcWCW[LW, RW] = Max[LW, RW] + 1
//        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
//        type CalcNCW[LW, RW] = Max[LW, RW]
//        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
//      }
//
//      trait DetailedBuilder[L, LW, LE, R, RW] {
//        type Out
//        def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out]
//      }
//      object DetailedBuilder {
//        implicit def ev[L, LW, LE, R, RW, NCW, WCW](
//          implicit
//          ctx : DFAny.Context,
//          ncW : Inference.NCW[LW, RW, NCW],
//          wcW : Inference.WCW[LW, RW, WCW],
//          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
//        ) : DetailedBuilder[L, LW, LE, R, RW]{type Out = Component[NCW, WCW]} =
//          new DetailedBuilder[L, LW, LE, R, RW]{
//            type Out = Component[NCW, WCW]
//            def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out] =
//              new Builder[L, LE, R] {
//                type Out = Component[NCW, WCW]
//                def apply(leftL : L, rightR : R) : Out = {
//                  import stdlib.DFUIntOps._
//                  val (creationKind, left, right) = properLR(leftL, rightR)
//                  // Completing runtime checks
//                  checkLWvRW.unsafeCheck(left.width, right.width)
//                  // Constructing op
//                  val opWidth = wcW(left.width, right.width)
//                  val opInst = creationKind match {
//                    case `Ops+Or-`.+ => `Func2Comp+`[LW, RW, WCW](left, right)
//                    case `Ops+Or-`.- => `Func2Comp-`[LW, RW, WCW](left, right)
//                  }
//                  opInst.__dev.setAutoName(if (opInst.usedAsWide) s"${ctx}WC" else s"${ctx}")
//                  // Creating extended component aliasing the op
//                  new Component[NCW, WCW](opInst)
//                }
//              }
//          }
//      }
//
//      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
//        implicit
//        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
//      ) = detailedBuilder((left, right) => (kind, left, right))
//
//      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, LE, R, RW](
//        implicit
//        ctx : DFAny.Context,
//        rConst : Const.PosNeg.Aux[R, RW],
//        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
//      ) = detailedBuilder((left, rightNum) => {
//        val (right, negative) = rConst(rightNum)
//        val creationKind = if (negative) -kind else kind
//        (creationKind, left, right)
//      })
//
//      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
//        implicit
//        ctx : DFAny.Context,
//        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
//        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
//      ) = detailedBuilder((leftNum, right) => {
//        (kind, lConst(leftNum), right)
//      })
//    }
//  }
//  protected object `Ops+Or-` {
//    abstract class Kind(val opString : String) {
//      def unary_- : Kind
//    }
//    case object + extends Kind("+") {def unary_- : Kind = `Ops+Or-`.-}
//    case object - extends Kind("-") {def unary_- : Kind = `Ops+Or-`.+}
//  }
//  object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
//  object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


}
