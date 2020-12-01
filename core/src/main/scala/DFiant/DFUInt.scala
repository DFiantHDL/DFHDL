package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.{Func2, Of}
import compiler.csprinter.CSPrinter

/**
  * A dataflow unsigned integer companion object
  */
object DFUInt extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token
    type TPattern = DFUInt.Pattern
    type TPatternAble[+R] = DFUInt.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFUInt.Pattern.Builder[LType]
    type `Op==Builder`[-L, -R] = DFUInt.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = DFUInt.`Op!=`.Builder[L, R]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = fromToken.toUInt
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case r @ DFUInt(_) =>
        import DFDesign.Frontend._
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
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Frontend extends Op.Implicits with `Op:=,<>`.Implicits with Token.Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenW[W] = DFAny.TokenT[Token, Type[W]]
  final case class Token(width : Int, value : Option[BigInt]) extends DFAny.Token.Of[Type[Int], BigInt] { left =>
    val dfType : DFAny.Type = Type(width)
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
      if (value < 0)
        throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      assert(value.bitsWidth(false) <= width, s"\nThe init value $value width must smaller or equal to $width")
      Token(width, Some(value))
    }
    def bubble(width : Int) : Token = Token(width, None)
    def apply(width : Int, token : Token) : Token = {
      assert(token.width <= width, s"\nThe init value $token width must smaller or equal to $width")
      Token(width, token.value)
    }

    type ToFit[LW, V] = DFAny.Token.ToFit.Summon.SAM[Type[LW], V, TokenW[LW]]
    type AsIs[LW, T, OW] = DFAny.Token.AsIs.Summon.Aux[Type[LW], T, Token, TokenW[OW]]
    trait Implicits {
      implicit def __DFUIntTokenAsIsInt[LW, V <: Int, OW](
        implicit
        natural : Natural.Int.CheckedShell[V],
        oWidthOf : BitsWidthOf.IntAux[V, OW]
      ) : AsIs[LW, V, OW] = new DFAny.Token.AsIs.Summon[Type[LW], V, Token] {
        type Out = TokenW[OW]
        def apply(from : Type[LW], value : V) : Out = {
          natural.unsafeCheck(value)
          Token(oWidthOf(value), value).typeTag[Type[OW]]
        }
      }
      implicit def __DFUIntTokenAsIsLong[LW, V <: Long, OW](
        implicit
        natural : Natural.Long.CheckedShell[V],
        oWidthOf : BitsWidthOf.LongAux[V, OW]
      ) : AsIs[LW, V, OW] = new DFAny.Token.AsIs.Summon[Type[LW], V, Token] {
        type Out = TokenW[OW]
        def apply(from : Type[LW], value : V) : Out = {
          natural.unsafeCheck(value)
          Token(oWidthOf(value), value).typeTag[Type[OW]]
        }
      }
      implicit def __DFUIntTokenAsIsBigInt[LW]
      : AsIs[LW, BigInt, Int] = new DFAny.Token.AsIs.Summon[Type[LW], BigInt, Token] {
        type Out = TokenW[Int]
        def apply(from : Type[LW], value : BigInt) : Out = {
          Natural.BigInt.unsafeCheck(value)
          Token(value.bitsWidth(false), value).typeTag[Type[Int]]
        }
      }
      implicit def __DFUIntTokenToFit[LW, V, RW](
        implicit
        summonedToken : AsIs[LW, V, RW],
        fitsWidth : `LW >= RW`.CheckedShell[LW, RW]
      ) : ToFit[LW, V] = (from, value) => {
        val token = summonedToken(from, value)
        fitsWidth.unsafeCheck(from.width, token.width)
        token.resize(from.width).typeTag[Type[LW]]
      }
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
      import DFDesign.Frontend._
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
          val width = TwoFace.Int(absValue.bitsWidth(false))
          (DFAny.Const[Type[W]](Type(width), Token(width, absValue)), value < 0)
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
    trait Implicits {
      final implicit def __DFUIntWiden[FW, TW](c : DFUInt[FW])(implicit eq : OpContainer.Eq[FW, TW, Int]) : DFUInt[TW] = c.asInstanceOf[DFUInt[TW]]
      sealed class __DFUIntFromInt[L <: Int](left : L) extends AbleOps[L](left)
      final implicit def __DFUIntFromInt[L <: Int](left: L): __DFUIntFromInt[L] = new __DFUIntFromInt(left)
      sealed class __DFUIntFromXInt[L <: XInt](left : L) extends AbleOps[ValueOf[L]](new ValueOf(left))
      final implicit def __DFUIntFromXInt[L <: XInt](left: L): __DFUIntFromXInt[L] = new __DFUIntFromXInt(left)
      sealed class __DFUIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def __DFUIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): __DFUIntFromLong[L] = new __DFUIntFromLong(left)
      sealed class __DFUIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends AbleOps[ValueOf[L]](new ValueOf(left))
      final implicit def __DFUIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): __DFUIntFromXLong[L] = new __DFUIntFromXLong(left)
      sealed class __DFUIntFromBigInt[L <: BigInt](left : L) extends AbleOps[L](left)
      final implicit def __DFUIntFromBigInt[L <: BigInt](left: L): __DFUIntFromBigInt[L] = new __DFUIntFromBigInt[L](left)
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
        def *   [R](right : Exact[R])(implicit op: `Op*`.Builder[DFUInt[LW], false, R]) = op(left, right)
        def *^  [R](right : Exact[R])(implicit op: `Op*^`.Builder[DFUInt[LW], true, R]) = op(left, right)
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
              DFAny.Const.forced[Type[RW]](token.resize(toWidth))
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
    trait Implicits {
      final implicit def __DFUInt_ac_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW >= RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFUInt[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        right.resize(left.width)
      }
      final implicit def __DFUInt_ac_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFBits[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        right.uint.asValOf[Type[LW]]
      }
      final implicit def __DFUInt_ac_DFBitsConst[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : DFBits.Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], R] = (left, rightR) => {
        val right = rConst(rightR)
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        right.uint.asValOf[Type[LW]]
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

      implicit def evDFUInt_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[Type[LW], R, _ <: Type[RW]],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(left.dfType, rightNum).asInstanceOf[DFUInt[RW]]
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.AsIs.Aux[Type[RW], L, _ <: Type[LW]],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(right.dfType, leftNum).asInstanceOf[DFUInt[LW]]
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
          doCheck : SafeBoolean[LE],
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
                  if (doCheck) checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val out = Type(opWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = updatedOp match {
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
        lConst : DFAny.Const.AsIs.Aux[Type[RW], L, _ <: Type[LW]],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (op, lConst(right.dfType, leftNum).asInstanceOf[DFUInt[LW]], right)
      })
    }
  }
  object `Op+`  extends `Ops+Or-`(Func2.Op.+)
  object `Op+^` extends `Ops+Or-`(Func2.Op.+^)
  object `Op-`  extends `Ops+Or-`(Func2.Op.-)
  object `Op-^` extends `Ops+Or-`(Func2.Op.-^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Op*`[Op <: Func2.Op.OptionalCarry](val op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = ITE[op.WC, LW + RW, Max[LW, RW]]
        type OutW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, OutW](
          implicit
          ctx : DFAny.Context,
          outW : Inference.OutW[LW, RW, OutW],
          doCheck : SafeBoolean[LE],
          checkLWvRW : `LW >= RW`.CheckedExtendable[LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Out = DFUInt[OutW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Out = DFUInt[OutW]
            def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFUInt[OutW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  if (doCheck) checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val out = Type(opWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = op match {
                    case _ : Func2.Op.* => _ * _
                    case _ : Func2.Op.*^ => _ *^ _
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
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFUInt_op_Const[LW, LE, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[Type[LW], R, _ <: Type[RW]],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(left.dfType, rightNum).asInstanceOf[DFUInt[RW]]))

      implicit def evConst_op_DFUInt[L, LW, LE, RW](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.AsIs.Aux[Type[RW], L, _ <: Type[LW]],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(right.dfType, leftNum).asInstanceOf[DFUInt[LW]], right))
    }
  }
  object `Op*`  extends `Op*`(Func2.Op.*)
  object `Op*^` extends `Op*`(Func2.Op.*^)
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
