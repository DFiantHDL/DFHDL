package DFiant

import DFiant.BasicLib._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import shapeless.<:!<

trait DFSInt[W] extends DFSInt.Unbounded {
  type Width = W
}

object DFSInt extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFSInt.type] {
    type LW = Width
    type TVal = DFSInt[LW]
    type TVar = DFSInt.Var[LW]
    type TToken = DFSInt.Token
    type TPattern = DFSInt.Pattern
    type TPatternAble[+R] = DFSInt.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFSInt.Pattern.Builder[L]

    lazy val sign = bits.setAnonymous().msbit.setAnonymous().setAutoConstructCodeString(s"$refCodeString.sign")

    def unary_- (implicit op: `Op-`.Builder[0, TVal]) = op(0, left)
    def +  [R](right: Op.Able[R])(implicit op: `Op+`.Builder[TVal, R]) = op(left, right)
    def -  [R](right: Op.Able[R])(implicit op: `Op-`.Builder[TVal, R]) = op(left, right)
    def *  [R](right: Op.Able[R])(implicit op: `Op*`.Builder[TVal, R]) = op(left, right)
    //  def /  (right : DFSInt)         : DFSInt = ???

    def <  [R](right: Op.Able[R])(implicit op: `Op<`.Builder[TVal, R]) = op(left, right)
    def >  [R](right: Op.Able[R])(implicit op: `Op>`.Builder[TVal, R]) = op(left, right)
    def <= [R](right: Op.Able[R])(implicit op: `Op<=`.Builder[TVal, R]) = op(left, right)
    def >= [R](right: Op.Able[R])(implicit op: `Op>=`.Builder[TVal, R]) = op(left, right)

    def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) = op(left, right)
    def == [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) = op(left, right)
    def == (that : BigInt)(implicit op: `Op==`.Builder[TVal, BigInt]) = op(left, that)
    def != [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) = op(left, right)
    def != [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) = op(left, right)
    def != (that : BigInt)(implicit op: `Op!=`.Builder[TVal, BigInt]) = op(left, that)


    def extendBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFSInt[tfs.Out] = {
      val extension = List.fill(numOfBits)(sign)
      new DFSInt.Alias[tfs.Out](extension :+ this, AliasReference.AsIs(s".bits.sint")).setAutoConstructCodeString(s"$refCodeString.extendBy($numOfBits)")
    }

    def extendTo[EW](numOfBits : ExtWidth.Checked[EW,LW])(implicit ctx : DFAny.Alias.Context)
    : DFSInt[EW] = {
      val extension = List.fill(width - numOfBits)(sign)
      new DFSInt.Alias[EW](extension :+ this, AliasReference.AsIs(s".bits.sint")).setAutoConstructCodeString(s"$refCodeString.extendTo($numOfBits)")
    }

    def isZero(implicit ctx : DFAny.Op.Context) = left == 0
    def isPositive(implicit ctx : DFAny.Op.Context) = left > 0
    def isNegative(implicit ctx : DFAny.Op.Context) = sign
    def isNonZero(implicit ctx : DFAny.Op.Context) = left != 0

    override lazy val typeName: String = s"DFSInt[$width]"
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFSInt[W] with DFAny.Var {}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit ctx : DFAny.NewVar.Context, checkedWidth : SIntWidth.Checked[W], di: DummyImplicit
  ) : NewVar[W] = new NewVar(checkedWidth)
  def apply[W](checkedWidth : SIntWidth.Checked[W])(
    implicit ctx : DFAny.NewVar.Context
  ) : NewVar[W] = new NewVar(checkedWidth.unsafeCheck())
  //  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : BigInt) : Var = apply(bigIntRepWidth(supLimit-1))
  //  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar(width, s"DFSInt($width)") with Var[W] {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](NewVar.this)
  }

  protected[DFiant] final class Alias[W](aliasedVars : List[DFAny], reference : AliasReference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFSInt[W]](aliasedVars, reference) with Var[W]

  protected[DFiant] final class Const[W](token : DFSInt.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFSInt[W]

  protected[DFiant] final class Port[W, Dir <: DFDir](dfVar : DFSInt[W], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFSInt[W], Dir](dfVar, dir) with DFSInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (width : Int, value : BigInt, val bubble : Boolean) extends DFAny.Token.Of[BigInt](width, value) {
    lazy val valueBits : BitVector = value.toBitVector(width)
    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
    def toBubbleToken : Token = Token(width, Bubble)
    def mkTokenS(that : Token, result : BigInt, resultWidth : Int) : Token = {
      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
      else Token(resultWidth, result)
    }

    final def + (that : Token) : Token = mkTokenS(that, this.value + that.value, scala.math.max(this.width, that.width) + 1)
    final def - (that : Token) : Token = mkTokenS(that, this.value - that.value, scala.math.max(this.width, that.width) + 1)
    final def * (that : Token) : Token = mkTokenS(that, this.value * that.value, this.width + that.width)
    final def / (that : Token) : Token = mkTokenS(that, this.value / that.value, this.width)
    final def % (that : Token) : Token = mkTokenS(that, this.value % that.value, that.width)
    final def <  (that : Token) : DFBool.Token = DFBool.Token(this.value < that.value, this.isBubble || that.isBubble)
    final def >  (that : Token) : DFBool.Token = DFBool.Token(this.value > that.value, this.isBubble || that.isBubble)
    final def <= (that : Token) : DFBool.Token = DFBool.Token(this.value <= that.value, this.isBubble || that.isBubble)
    final def >= (that : Token) : DFBool.Token = DFBool.Token(this.value >= that.value, this.isBubble || that.isBubble)
    final def == (that : Token) : DFBool.Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    final def != (that : Token) : DFBool.Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    def +  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l + r)
    def -  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l - r)
    def *  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l * r)
    def /  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l / r)
    def %  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l % r)
    def <  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l < r)
    def >  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l > r)
    def <= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l <= r)
    def >= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l >= r)
    def == (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply(width : Int, value : Int) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : Long) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : BigInt) : Token = new Token(width, value, false)
    def apply(width : Int, value : Bubble) : Token = new Token(width, 0, true)
    def apply(width : Int, token : Token) : Token = {
      //TODO: Boundary checks
      new Token(width, token.value, token.bubble)
    }
    implicit val fromBits : DFBits.Token => Token = t => t.toSInt
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[LW, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFSInt[LW], Dir] = (right, dir) => new Port[LW, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.Width](List(left), AliasReference.AsIs(s".as(DFSInt(${mold.width}))"))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntWithinWidth[LW] = CompileTime[BitsWidthOf.Signed.CalcInt[GetArg0] <= LW]
      private type LongWithinWidth[LW] = CompileTime[BitsWidthOf.Signed.CalcLong[GetArg0] <= LW]
      implicit class DFSIntBubble[LW](val right : Bubble) extends Able[DFSInt[LW]]
      implicit class DFSIntToken[LW](val right : DFSInt.Token) extends Able[DFSInt[LW]]
      implicit class DFSIntTokenSeq[LW](val right : Seq[DFSInt.Token]) extends Able[DFSInt[LW]]
      implicit class DFSIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntBigInt[LW](val right : BigInt) extends Able[DFSInt[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFSInt[LW]]]) : Seq[Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFSInt.Token(width, t)
          case (t : DFSInt.Token) => DFSInt.Token(width, t)
          case (t : Int) => DFSInt.Token(width, t)
          case (t : Long) => DFSInt.Token(width, t)
          case (t : BigInt) => DFSInt.Token(width, t)
        })

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFSInt[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
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
      implicit class DFSIntPatternInt[R <: Int](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFULongPatternLong[R <: Long](val right : R)(implicit di : DummyImplicit) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFSIntPatternBigInt[R <: BigInt](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(right)
      }
      implicit class DFSIntPatternRange[R <: Range](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.fromRange(right).toBigIntInterval
      }
      implicit class DFSIntPatternIntervalInt[R <: Interval[Int]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalLong[R <: Interval[Long]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalBigInt[R <: Interval[BigInt]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right
      }
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[LW] : Builder[DFSInt[LW]] = new Builder[DFSInt[LW]] {
        def apply[R](left: DFSInt[LW], right: Seq[Able[R]]): Pattern = {
          val reqInterval = IntervalSet(Interval.closed(BigInt.minSignedFromWidth(left.width), BigInt.maxSignedFromWidth(left.width)))
          val patternSet = right.map(e => e.interval).foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
            if (set.intersect(interval).nonEmpty) throw new IllegalArgumentException(s"\nThe interval $interval already intersects with $set")
            if (!reqInterval.contains(interval)) throw new IllegalArgumentException(s"\nThe interval $interval is outside of range allowed by ${left.name}: $reqInterval")
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
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends PrevCO {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[LW](implicit ctx : DFAny.Alias.Context) : Builder[DFSInt[LW]] = new Builder[DFSInt[LW]] {
        def apply[P](left : DFSInt[LW], right : Natural.Int.Checked[P]) : DFSInt[LW] =
          new Alias(List(left), AliasReference.Prev(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def +  [RW](right : DFSInt[RW])(implicit op: `Op+`.Builder[L, DFSInt[RW]]) = op(left, right)
      def -  [RW](right : DFSInt[RW])(implicit op: `Op-`.Builder[L, DFSInt[RW]]) = op(left, right)
      def <  [RW](right : DFSInt[RW])(implicit op: `Op<`.Builder[L, DFSInt[RW]]) = op(left, right)
      def >  [RW](right : DFSInt[RW])(implicit op: `Op>`.Builder[L, DFSInt[RW]]) = op(left, right)
      def <= [RW](right : DFSInt[RW])(implicit op: `Op<=`.Builder[L, DFSInt[RW]]) = op(left, right)
      def >= [RW](right : DFSInt[RW])(implicit op: `Op>=`.Builder[L, DFSInt[RW]]) = op(left, right)
      def <> [RW, RDIR <: DFDir](port : DFSInt[RW] <> RDIR)(
        implicit op: `Op<>`.Builder[DFSInt[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
      def toDFSInt[LW](implicit op : Const.Builder.Aux[L, LW]) = op(left)
    }
    trait Implicits {
      sealed class DFSIntFromInt[L <: Int](left : L) extends Able[L](left)
      final implicit def DFSIntFromInt[L <: Int](left: L): DFSIntFromInt[L] = new DFSIntFromInt(left)
      sealed class DFSIntFromXInt[L <: XInt](left : L) extends Able[L](left)
      final implicit def DFSIntFromXInt[L <: XInt](left: L): DFSIntFromXInt[L] = new DFSIntFromXInt(left)
      sealed class DFSIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFSIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFSIntFromLong[L] = new DFSIntFromLong(left)
      sealed class DFSIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFSIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFSIntFromXLong[L] = new DFSIntFromXLong(left)
      sealed class DFSIntFromBigInt[L <: BigInt](left : L) extends Able[L](left)
      final implicit def DFSIntFromBigInt[L <: BigInt](left: L): DFSIntFromBigInt[L] = new DFSIntFromBigInt[L](left)
      final implicit def ofDFSInt[R <: DFSInt.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFSInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : DFSInt[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Signed.Int[N])
      : Aux[N, w.Out] = new Builder[N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = new Const[W](Token(w(value), value))
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Signed.Long[N])
      : Aux[N, w.Out] = new Builder[N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = new Const[W](Token(w(value), value))
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[N, Int] = new Builder[N] {
        type W = Int
        def apply(value : N) : DFSInt[W] = new Const[W](Token(value.bitsWidth, value))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>`[Ctx, SkipLengthCheck] extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = SkipLengthCheck || (LW >= RW)
        type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, R, RW](properR : (L, R) => DFSInt[RW]) : Aux[L, R, DFSInt[RW]] =
        new Builder[L, R] {
          type Comp = DFSInt[RW]
          def apply(leftL : L, rightR : R) : Comp =  properR(leftL, rightR)
        }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        ctx : Ctx,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFSInt[LW], DFSInt[RW], DFSInt[RW]] =
        create[DFSInt[LW], DFSInt[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : Ctx,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFSInt[LW], R, DFSInt[RW]] = create[DFSInt[LW], R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`[DFAny.Op.Context, false]
  object `Op<>` extends `Ops:=,<>`[DFAny.Connector.Context, true]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`(kind : DiSoOp.Kind) {
    //NCW = No-carry width
    //WCW = With-carry width
    class Component[NCW, WCW](val wc : DFSInt[WCW])(implicit ctx : DFAny.Alias.Context) extends
      DFAny.Alias[DFSInt[NCW]](List(wc), AliasReference.BitsWL(wc.width-1, 0, s".bits(${wc.width-2}, 0).sint")) with DFSInt[NCW] {
      lazy val c = new DFBool.Alias(List(wc), AliasReference.BitsWL(1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx.getName}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = Max[LW, RW] + 1
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
        ) : DetailedBuilder[L, LW, R, RW]{type Comp = Component[NCW, WCW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Comp = Component[NCW, WCW]
            def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, R, Comp] =
              new Builder[L, R] {
                type Comp = Component[NCW, WCW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFSIntOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val opWidth = wcW(left.width, right.width)
                  val opInst = kind match {
                    case DiSoOp.Kind.+ => new DFiant.BasicLib.DFSIntOps.`Comp+`(left.width, right.width, opWidth)
                    case DiSoOp.Kind.- => new DFiant.BasicLib.DFSIntOps.`Comp-`(left.width, right.width, opWidth)
                    case _ => throw new IllegalArgumentException("Unexpected operation")
                  }
                  opInst.setAutoName(s"${ctx.getName}Comp")
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFSInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.getName}WC")
                  // Creating extended component aliasing the op
                  new Component[NCW, WCW](wc)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFSInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  object `Op+` extends `Ops+Or-`(DiSoOp.Kind.+)
  object `Op-` extends `Ops+Or-`(DiSoOp.Kind.-)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // * operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op*` {
    //NCW = No-carry width
    //WCW = With-carry width
    //CW = Carry width
    class Component[NCW, WCW, CW](val wc : DFSInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(
      implicit ctx : DFAny.Alias.Context
    ) extends DFAny.Alias[DFSInt[NCW]](List(wc), AliasReference.BitsWL(ncW, 0, s".bits(${wc.width-cW-1}, 0).sint")) with DFSInt[NCW] {
      lazy val c = new DFBits.Alias[CW](List(wc), AliasReference.BitsWL(cW, wc.width - cW, s".bits(${wc.width-1}, ${wc.width-cW})")).setAutoName(s"${ctx.getName}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Op `*` with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = LW + RW
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
        type CalcCW[LW, RW] = CalcWCW[LW, RW] - CalcNCW[LW, RW]
        type CW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, CW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          cW : Inference.CW[LW, RW, CW],
        ) : DetailedBuilder[L, LW, R, RW]{type Comp = Component[NCW, WCW, CW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Comp = Component[NCW, WCW, CW]
            def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, R, Comp] =
              new Builder[L, R] {
                type Comp = Component[NCW, WCW, CW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFSIntOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val wcWidth = wcW(left.width, right.width)
                  val ncWidth = ncW(left.width, right.width)
                  val cWidth = cW(left.width, right.width)

                  val opInst = new DFiant.BasicLib.DFSIntOps.`Comp*`(left.width, right.width, wcWidth)
                  opInst.setAutoName(s"${ctx.getName}Comp")
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFSInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.getName}WC")

                  // Creating extended component aliasing the op
                  new Component[NCW, WCW, CW](wc, ncWidth, cWidth)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFSInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFSInt.Token], Seq[DFSInt.Token]) => Seq[DFBool.Token]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFSInt[LW], DFSInt[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import ctx.basicLib.DFSIntOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => new DFiant.BasicLib.DFSIntOps.`Comp==`(left.width, right.width)
          case DiSoOp.Kind.!= => new DFiant.BasicLib.DFSIntOps.`Comp!=`(left.width, right.width)
          case DiSoOp.Kind.<  => new DFiant.BasicLib.DFSIntOps.`Comp<`(left.width, right.width)
          case DiSoOp.Kind.>  => new DFiant.BasicLib.DFSIntOps.`Comp>`(left.width, right.width)
          case DiSoOp.Kind.<= => new DFiant.BasicLib.DFSIntOps.`Comp<=`(left.width, right.width)
          case DiSoOp.Kind.>= => new DFiant.BasicLib.DFSIntOps.`Comp>=`(left.width, right.width)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.setAutoName(s"${ctx.getName}Comp")
        opInst.inLeft <> left
        opInst.inRight <> right
        opInst.outResult
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFSInt[LW], DFSInt[RW]] = create[DFSInt[LW], LW, DFSInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
      ) : Builder[DFSInt[LW], R] = create[DFSInt[LW], LW, R, RW]((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
      ) : Builder[L, DFSInt[RW]] = create[L, LW, DFSInt[RW], RW]((leftNum, right) => (lConst(leftNum), right))
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFSInt.Token.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFSInt.Token.!=) with `Op!=`
  object `Op<`  extends OpsCompare(DiSoOp.Kind.< )(DFSInt.Token.< )
  object `Op>`  extends OpsCompare(DiSoOp.Kind.> )(DFSInt.Token.> )
  object `Op<=` extends OpsCompare(DiSoOp.Kind.<=)(DFSInt.Token.<=)
  object `Op>=` extends OpsCompare(DiSoOp.Kind.>=)(DFSInt.Token.>=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}