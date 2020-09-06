package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.{Func2, Of}
import compiler.csprinter.CSPrinter

object DFUInt extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token
    type TPattern = DFUInt.Pattern
    type TPatternAble[+R] = DFUInt.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFUInt.Pattern.Builder[LType]
    type `Op==Builder`[-L, -R] = DFUInt.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = DFUInt.`Op!=`.Builder[L, R]
    type InitAble[L <: DFAny] = DFUInt.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFUInt.Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = fromToken.toUInt
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case r @ DFUInt(_) =>
        import DFDesign.Implicits._
        val op = implicitly[DFAny.`Op:=,<>`.Builder[Type[W], DFUInt[Int]]]
        op(this, r.asValOf[Type[Int]])
    }
    override def toString: String = s"DFUInt[$width]"
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"$TP DFUInt($LIT$width)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(width) => this.width.getValue == width.getValue
      case _ => false
    }
  }
  trait Extendable

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : BitsWidth.Checked[W])(implicit ctx : DFAny.Context) : DFAny.NewVar[Type[W]] =
    DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def apply[W](
    implicit ctx : DFAny.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : DFAny.NewVar[Type[W]] = DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def unapply(arg: DFAny.Member): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
    case _ => None
  }
  def max[U](maxValue : Positive.Checked[U])(
    implicit ctx : DFAny.Context, w : BitsWidthOf.Int[U]
  ) : DFAny.NewVar[Type[w.Out]] = DFAny.NewVar(Type(w(maxValue.getValue)))
  def until[U](supremum : Positive.Checked[U])(
    implicit ctx : DFAny.Context, w : BitsWidthOf.Int[U-1]
  ) : DFAny.NewVar[Type[w.Out]] = DFAny.NewVar(Type(w(supremum.getValue-1)))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token(width : Int, value : Option[BigInt]) extends DFAny.Token.Of[Type[Int], BigInt] { left =>
    private def mkTokenU(right : Token, f : (BigInt, BigInt) => BigInt, resultWidth : Int) : Token = {
      (left.value, right.value) match {
        case (Some(l), Some(r)) => Token(resultWidth, f(l, r).asUnsigned(resultWidth))
        case _ => Token.bubble(resultWidth)
      }
    }

    def +  (right : Token) : Token = mkTokenU(right, _ + _, left.width max right.width)
    def +^ (right : Token) : Token = mkTokenU(right, _ + _, (left.width max right.width) + 1)
    def -  (right : Token) : Token = mkTokenU(right, _ - _, left.width max right.width)
    def -^ (right : Token) : Token = mkTokenU(right, _ - _, (left.width max right.width) + 1)
    def *  (right : Token) : Token = mkTokenU(right, _ * _, left.width max right.width)
    def *^ (right : Token) : Token = mkTokenU(right, _ * _, left.width + right.width)
    def /  (right : Token) : Token = mkTokenU(right, _ / _, left.width)
    def %  (right : Token) : Token = mkTokenU(right, _ % _, right.width)

    def <  (right : Token) : DFBool.Token = mkTokenB(right, _ < _)
    def >  (right : Token) : DFBool.Token = mkTokenB(right, _ > _)
    def <= (right : Token) : DFBool.Token = mkTokenB(right, _ <= _)
    def >= (right : Token) : DFBool.Token = mkTokenB(right, _ >= _)
    def == (right : Token) : DFBool.Token = mkTokenB(right, _ == _)
    def != (right : Token) : DFBool.Token = mkTokenB(right, _ != _)
    def << (right : DFUInt.Token) : Token = (left.bits << right).toUInt
    def >> (right : DFUInt.Token) : Token = (left.bits >> right).toUInt
    def resize(toWidth : Int) : Token = left.bits.resize(toWidth).toUInt
    def valueToBitVector(value : BigInt) : BitVector = value.toBitVector(width)
    def valueCodeString(value : BigInt)(implicit printer: CSPrinter) : String = {
      import printer.config._
      if (value.isValidInt) s"$LIT$value"
      else if (value.isValidLong) s"$LIT${value}L"
      else s"""$LIT BigInt($STR"$value")"""
    }
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token.bubble(t.width)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => Token.bubble(t.width)
    def apply(width : Int, value : Int) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : Long) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : BigInt) : Token = {
      if (value < 0) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      assert(value.bitsWidth <= width, s"\nThe init value $value width must smaller or equal to $width")
      Token(width, Some(value))
    }
    def bubble(width : Int) : Token = Token(width, None)
    def apply(width : Int, token : Token) : Token = {
      assert(token.width <= width, s"\nThe init value $token width must smaller or equal to $width")
      Token(width, token.value)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(intervalSet : IntervalSet[BigInt]) extends DFAny.Pattern.OfIntervalSet[Type[Int], BigInt, Pattern](intervalSet) {
    protected def matchCond(matchVal: DFAny.Of[Type[Int]], interval : Interval[BigInt])(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Implicits._
      import continuum.bound._
      val (lower, lowerCond) = interval.lower.bound match {
        case Closed(v) => (v, matchVal >= v)
        case Open(v) => (v + 1, matchVal > v)
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      val (upper, upperCond) = interval.upper.bound match {
        case Closed(v) => (v, matchVal <= v)
        case Open(v) => (v - 1, matchVal < v)
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      if (lower == upper) (matchVal === lower).anonymize
      else (lowerCond.anonymize && upperCond.anonymize).anonymize
    }
  }
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
      implicit class DFUIntToken[LW](val right : DFUInt.Token) extends Able[DFUInt[LW]]
      implicit class DFUIntTokenSeq[LW](val right : Seq[DFUInt.Token]) extends Able[DFUInt[LW]]
      implicit class DFUIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntBigInt[LW](val right : BigInt) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfInt[LW](val right : Seq[Int]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfLong[LW](val right : Seq[Long]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfBigInt[LW](val right : Seq[BigInt]) extends Able[DFUInt[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFUInt[LW]]]) : Seq[Token] =
        right.toSeqAny.collect {
          case (t : Bubble) => DFUInt.Token.bubble(width)
          case (t : Token) => assert(t.width == width); t
          case (t : Int) => DFUInt.Token(width, t)
          case (t : Long) => DFUInt.Token(width, t)
          case (t : BigInt) => DFUInt.Token(width, t)
        }

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFUInt[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
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
      implicit def fromValueOf[N](
        implicit const : PosNeg[N]
      ) : Aux[ValueOf[N], const.W] = new PosNeg[ValueOf[N]] {
        type W = const.W
        def apply(value : ValueOf[N]) : (DFUInt[W], Boolean) = const(value.value)
      }
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Context, w : BitsWidthOf.Int[Abs[N]], di : DummyImplicit)
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          val width = w(absValue)
          (DFAny.Const[Type[W]](Type(width), Token(width, absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Context, w : BitsWidthOf.Long[Abs[N]], di : DummyImplicit)
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
    trait NatOnly[N] {
      type W
      def apply(value : N) : DFUInt[W]
    }
    object NatOnly {
      type Aux[N, W0] = NatOnly[N]{type W = W0}
      object `N >= 0` extends `N >= 0` {
        type MsgCommon[N] = "Operation or assignment do not permit a negative number. Found literal: " + ToString[N]
      }
      implicit def fromValueOf[N](
        implicit const : NatOnly[N]
      ) : Aux[ValueOf[N], const.W] = new NatOnly[ValueOf[N]] {
        type W = const.W
        def apply(value : ValueOf[N]) : DFUInt[W] = const(value.value)
      }
      implicit def fromInt[N <: Int](
        implicit
        ctx : DFAny.Context,
        checkPos : `N >= 0`.Int.CheckedShell[N],
        w : BitsWidthOf.Int[N],
        di : DummyImplicit
      ) : Aux[N, w.Out] = new NatOnly[N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromLong[N <: Long](
        implicit
        ctx : DFAny.Context,
        checkPos : `N >= 0`.Long.CheckedShell[N],
        w : BitsWidthOf.Long[N],
        di : DummyImplicit
      ) : Aux[N, w.Out] = new NatOnly[N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Context)
      : Aux[N, Int] = new NatOnly[N] {
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
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
      final def +   [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, false, DFUInt[RW]]) = op(left, right)
      final def +^  [RW](right : DFUInt[RW])(implicit op: `Op+^`.Builder[L, true, DFUInt[RW]]) = op(left, right)
      final def -   [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, false, DFUInt[RW]]) = op(left, right)
      final def -^  [RW](right : DFUInt[RW])(implicit op: `Op-^`.Builder[L, true, DFUInt[RW]]) = op(left, right)
      final def <   [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def >   [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def <=  [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def >=  [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def === [RW](right : DFUInt[RW])(implicit op: `Op===`.Builder[L, DFUInt[RW]]) = op(left, right)
      final def =!= [RW](right : DFUInt[RW])(implicit op: `Op=!=`.Builder[L, DFUInt[RW]]) = op(left, right)
    }
    trait Implicits extends `Op:=,<>`.Implicits {
      final implicit def __DFUIntWiden[FW, TW](c : DFUInt[FW])(implicit eq : OpContainer.Eq[FW, TW, Int]) : DFUInt[TW] = c.asInstanceOf[DFUInt[TW]]
      sealed class __DFUIntFromInt[L <: Int](left : L) extends AbleOps[L](left)
      final implicit def __DFUIntFromInt[L <: Int](left: L): __DFUIntFromInt[L] = new __DFUIntFromInt(left)
      sealed class __DFUIntFromXInt[L <: XInt](left : L) extends AbleOps[L](left)
      final implicit def __DFUIntFromXInt[L <: XInt](left: L): __DFUIntFromXInt[L] = new __DFUIntFromXInt(left)
      sealed class __DFUIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def __DFUIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): __DFUIntFromLong[L] = new __DFUIntFromLong(left)
      sealed class __DFUIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def __DFUIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): __DFUIntFromXLong[L] = new __DFUIntFromXLong(left)
      sealed class __DFUIntFromBigInt[L <: BigInt](left : L) extends AbleOps[L](left)
      final implicit def __DFUIntFromBigInt[L <: BigInt](left: L): __DFUIntFromBigInt[L] = new __DFUIntFromBigInt[L](left)
      sealed class __DFUIntFromValueOf[W, T <: DFUInt[W]](left : ValueOf[T])(implicit ctx : DFAny.Context) extends AbleOps[T](left)
      final implicit def __DFUIntFromValueOf[W, T <: DFUInt[W]](left : ValueOf[T])(implicit ctx : DFAny.Context) : __DFUIntFromValueOf[W, T] = new __DFUIntFromValueOf[W, T](left)
      final implicit def __ofDFUInt[W](left : DFUInt[W]) : Able[DFUInt[W]] = new Able(left)
      final implicit class __ExtendableDFUIntOps[LW](val left : DFUInt[LW] with Extendable){
        def +  [R](right : Exact[R])(implicit op: `Op+`.Builder[DFUInt[LW], true, R]) = op(left, right)
        def -  [R](right : Exact[R])(implicit op: `Op-`.Builder[DFUInt[LW], true, R]) = op(left, right)
      }
      final implicit class __DFUIntOps[LW](val left : DFUInt[LW]){
        def +   [R](right : Exact[R])(implicit op: `Op+`.Builder[DFUInt[LW], false, R]) = op(left, right)
        def +^  [R](right : Exact[R])(implicit op: `Op+^`.Builder[DFUInt[LW], true, R]) = op(left, right)
        def -   [R](right : Exact[R])(implicit op: `Op-`.Builder[DFUInt[LW], false, R]) = op(left, right)
        def -^  [R](right : Exact[R])(implicit op: `Op-^`.Builder[DFUInt[LW], true, R]) = op(left, right)
        def <   [R](right : Exact[R])(implicit op: `Op<`.Builder[DFUInt[LW], R]) = op(left, right)
        def >   [R](right : Exact[R])(implicit op: `Op>`.Builder[DFUInt[LW], R]) = op(left, right)
        def <=  [R](right : Exact[R])(implicit op: `Op<=`.Builder[DFUInt[LW], R]) = op(left, right)
        def >=  [R](right : Exact[R])(implicit op: `Op>=`.Builder[DFUInt[LW], R]) = op(left, right)
        def === [R](right : Exact[R])(implicit op: `Op===`.Builder[DFUInt[LW], R]) = op(left, right)
        def =!= [R](right : Exact[R])(implicit op: `Op=!=`.Builder[DFUInt[LW], R]) = op(left, right)
        def << [R](right: Exact[R])(implicit op: `Op<<`.Builder[DFUInt[LW], R]) = op(left, right)
        def >> [R](right: Exact[R])(implicit op: `Op>>`.Builder[DFUInt[LW], R]) = op(left, right)
        def resize[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFUInt[RW] =
          left.member match {
            case DFAny.Const(_, token : Token, _, _) =>
              DFAny.Const.forced(Type(toWidth), token.resize(toWidth))
            case _ =>
              if (left.width.getValue == toWidth.getValue) left.asInstanceOf[DFUInt[RW]]
              else DFAny.Alias.Resize.uint(left.member, toWidth)
          }

        def extendable : DFUInt[LW] with Extendable = left.asInstanceOf[DFUInt[LW] with Extendable]
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    import DFAny.`Op:=,<>`.Builder
    object `LW >= RW` extends Checked1Param.Int {
      type Cond[LW, RW] = LW >= RW
      type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
      type ParamFace = Int
    }

    trait Implicits {
      final implicit def __DFUInt_ac_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW >= RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFUInt[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Implicits._
        right.resize(left.width)
      }

      final implicit def __DFUInt_ac_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.NatOnly.Aux[R, RW],
        checkLWvRW : `LW >= RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Implicits._
        right.resize(left.width)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
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
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFUInt[LW], DFUInt[RW]] = create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.NatOnly.Aux[R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.NatOnly.Aux[L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==`  extends OpsCompare(Func2.Op.==)(_ == _) with `Op==`
  object `Op!=`  extends OpsCompare(Func2.Op.!=)(_ != _) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)(_ == _)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)(_ != _)
  object `Op<`   extends OpsCompare(Func2.Op.< )(_ <  _)
  object `Op>`   extends OpsCompare(Func2.Op.> )(_ >  _)
  object `Op<=`  extends OpsCompare(Func2.Op.<=)(_ <= _)
  object `Op>=`  extends OpsCompare(Func2.Op.>=)(_ >= _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[Op <: Func2.Op.Negateable](val op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[LW, LE, RW] = CheckedShell[LW, ITE[LE, 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = Max[LW, RW] + ITE[op.WC, 1, 0]
        type OutW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Out
        def apply(properLR : (L, R) => (Func2.Op.Negateable, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, OutW](
          implicit
          ctx : DFAny.Context,
          outW : Inference.OutW[LW, RW, OutW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Out = DFUInt[OutW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Out = DFUInt[OutW]
            def apply(properLR : (L, R) => (Func2.Op.Negateable, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFUInt[OutW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (updatedOp, left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val out = Type(opWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = op match {
                    case _ : Func2.Op.+ => _ + _
                    case _ : Func2.Op.- => _ - _
                    case _ : Func2.Op.+^ => _ +^ _
                    case _ : Func2.Op.-^ => _ -^ _
                    case _ => ???
                  }
                  DFAny.Func2(out, left, op, right)(func)
                }
              }
          }
      }

      implicit def evDFUInt_op_DFUInt[LW, LE, RW](
        implicit
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((left, right) => (op, left, right))

      implicit def evDFUInt_op_Const[LW, LE, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.PosNeg.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val (right, negative) = rConst(rightNum)
        val updatedOp : Func2.Op.Negateable = if (negative) op.negate else op
        (updatedOp, left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, LE, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.NatOnly.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (op, lConst(leftNum), right)
      })
    }
  }
  object `Op+`  extends `Ops+Or-`(Func2.Op.+)
  object `Op+^` extends `Ops+Or-`(Func2.Op.+^)
  object `Op-`  extends `Ops+Or-`(Func2.Op.-)
  object `Op-^` extends `Ops+Or-`(Func2.Op.-^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op<<` extends OpsShift[Type](Func2.Op.<<) {
    def tokenFunc[LW](left: DFUInt.Token, right: DFUInt.Token) : DFUInt.Token = left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW](left: DFUInt.Token, right: DFUInt.Token) : DFUInt.Token = left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


}
