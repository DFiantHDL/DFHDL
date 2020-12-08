package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.Func2
import compiler.csprinter.CSPrinter

/**
  * A dataflow signed integer companion object
  */
object DFSInt extends DFAny.Companion {
  final case class Type[W](width: TwoFace.Int[W]) extends DFAny.Type {
    type Width                                = W
    type TToken                               = Token
    type TPattern                             = DFSInt.Pattern
    type TPatternAble[+R]                     = DFSInt.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFSInt.Pattern.Builder[LType]
    type `Op==Builder`[-L, -R]                = DFSInt.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R]                = DFSInt.`Op!=`.Builder[L, R]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken: DFBits.Token): DFAny.Token =
      fromToken.toSInt
    override def toString: String = s"DFSInt[$width]"
    def assignCheck(from: DFAny.Member)(implicit ctx: DFAny.Context): Unit =
      from match {
        case r @ DFSInt(_) =>
          import DFDesign.Frontend._
          val op = implicitly[DFAny.`Op:=,<>`.Builder[Type[W], DFSInt[Int]]]
          op(this, r.asValOf[Type[Int]])
      }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"$TP DFSInt($LIT$width)"
    }
    override def equals(obj: Any): Boolean =
      obj match {
        case Type(width) => this.width.getValue == width.getValue
        case _           => false
      }
  }
  trait Extendable

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](
      checkedWidth: SIntWidth.Checked[W]
  )(implicit ctx: DFAny.Context): DFAny.NewVar[Type[W]] =
    DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def apply[W](implicit
      ctx: DFAny.Context,
      checkedWidth: SIntWidth.Checked[W],
      di: DummyImplicit
  ): DFAny.NewVar[Type[W]] = DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def unapply(arg: DFAny.Member): Option[Int] =
    arg.dfType match {
      case Type(width) => Some(width.getValue)
      case _           => None
    }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Frontend
      extends Op.Implicits
      with `Op:=,<>`.Implicits
      with Token.Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenW[W] = DFAny.TokenT[Token, Type[W]]
  final case class Token(width: Int, value: Option[BigInt])
      extends DFAny.Token.Of[Type[Int], BigInt] { left =>
    val dfType: DFAny.Type = Type(width)
    private def mkTokenS[T <: DFAny.Type](
        right: DFAny.Token.Of[T, BigInt],
        f: (BigInt, BigInt) => BigInt,
        resultWidth: Int
    ): Token = {
      (left.value, right.value) match {
        case (Some(l), Some(r)) => Token(resultWidth, f(l, r))
        case _                  => Token.bubble(resultWidth)
      }
    }
    def +(right: Token): Token =
      mkTokenS(right, _ + _, left.width max right.width)
    def +^(right: Token): Token =
      mkTokenS(right, _ + _, (left.width max right.width) + 1)
    def -(right: Token): Token =
      mkTokenS(right, _ - _, left.width max right.width)
    def -^(right: Token): Token =
      mkTokenS(right, _ - _, (left.width max right.width) + 1)
    def *(right: Token): Token =
      mkTokenS(right, _ * _, left.width max right.width)
    def *^(right: Token): Token =
      mkTokenS(right, _ * _, left.width + right.width)
    def /(right: Token): Token = mkTokenS(right, _ / _, left.width)
    def %(right: Token): Token = mkTokenS(right, _ % _, right.width)

    def <(right: Token): DFBool.Token  = mkTokenB(right, _ < _)
    def >(right: Token): DFBool.Token  = mkTokenB(right, _ > _)
    def <=(right: Token): DFBool.Token = mkTokenB(right, _ <= _)
    def >=(right: Token): DFBool.Token = mkTokenB(right, _ >= _)
    def ==(right: Token): DFBool.Token = mkTokenB(right, _ == _)
    def !=(right: Token): DFBool.Token = mkTokenB(right, _ != _)
    def <<(right: DFUInt.Token): Token = mkTokenS(right, _ << _.toInt, width)
    def >>(right: DFUInt.Token): Token = mkTokenS(right, _ >> _.toInt, width)
    def resize(toWidth: Int): Token = {
      if (toWidth > left.width) Token(toWidth, left.value)
      else if (toWidth < left.width) left.bits.resize(toWidth).toSInt
      else this.asInstanceOf[Token]
    }
    def valueToBitVector(value: BigInt): BitVector = value.toBitVector(width)
    def valueCodeString(value: BigInt)(implicit printer: CSPrinter): String = {
      import printer.config._
      if (value.isValidInt) s"$LIT$value"
      else if (value.isValidLong) s"$LIT${value}L"
      else s"""$LIT BigInt($STR"$value")"""
    }
  }

  object Token {
    implicit val bubbleOfToken: DFAny.Token.BubbleOfToken[Token] = t =>
      Token.bubble(t.width)
    implicit def bubbleOfDFType[W]: DFAny.Token.BubbleOfDFType[Type[W]] =
      t => Token.bubble(t.width)

    def apply(width: Int, value: Int): Token  = Token(width, BigInt(value))
    def apply(width: Int, value: Long): Token = Token(width, BigInt(value))
    def apply(width: Int, value: BigInt): Token = {
      assert(
        value.bitsWidth(true) <= width,
        s"\nThe init value $value width must smaller or equal to $width"
      )
      Token(width, Some(value))
    }
    def bubble(width: Int): Token = Token(width, None)
    def apply(width: Int, token: Token): Token = {
      assert(
        token.width <= width,
        s"\nThe init value $token width must smaller or equal to $width"
      )
      Token(width, token.value)
    }

    type ToFit[LW, V] = DFAny.Token.ToFit.Summon.SAM[Type[LW], V, TokenW[LW]]
    type AsIs[LW, T, OW] =
      DFAny.Token.AsIs.Summon.Aux[Type[LW], T, Token, TokenW[OW]]
    trait Implicits {
      implicit def __DFSIntTokenAsIsInt[LW, V <: Int, OW](implicit
          oWidthOf: BitsWidthOf.Signed.IntAux[V, OW]
      ): AsIs[LW, V, OW] =
        new DFAny.Token.AsIs.Summon[Type[LW], V, Token] {
          type Out = TokenW[OW]
          def apply(from: Type[LW], value: V): Out = {
            Token(oWidthOf(value), value).typeTag[Type[OW]]
          }
        }
      implicit def __DFSIntTokenAsIsLong[LW, V <: Long, OW](implicit
          oWidthOf: BitsWidthOf.Signed.LongAux[V, OW]
      ): AsIs[LW, V, OW] =
        new DFAny.Token.AsIs.Summon[Type[LW], V, Token] {
          type Out = TokenW[OW]
          def apply(from: Type[LW], value: V): Out = {
            Token(oWidthOf(value), value).typeTag[Type[OW]]
          }
        }
      implicit def __DFSIntTokenAsIsBigInt[LW, OW]: AsIs[LW, BigInt, Int] =
        new DFAny.Token.AsIs.Summon[Type[LW], BigInt, Token] {
          type Out = TokenW[Int]
          def apply(from: Type[LW], value: BigInt): Out = {
            Token(value.bitsWidth(true), value).typeTag[Type[Int]]
          }
        }
      implicit def __DFSIntTokenToFit[LW, V, RW](implicit
          summonedToken: AsIs[LW, V, RW],
          fitsWidth: `LW >= RW`.CheckedShell[LW, RW]
      ): ToFit[LW, V] =
        (from, value) => {
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
  class Pattern(intervalSet: IntervalSet[BigInt])
      extends DFAny.Pattern.OfIntervalSet[Type[Int], BigInt, Pattern](
        intervalSet
      ) {
    protected def matchCond(
        matchVal: DFAny.Of[Type[Int]],
        interval: Interval[BigInt]
    )(implicit
        ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Frontend._
      import continuum.bound._
      val (lower, lowerCond) = interval.lower.bound match {
        case Closed(v) => (v, matchVal >= v)
        case Open(v)   => (v + 1, matchVal > v)
        case Unbounded() =>
          throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      val (upper, upperCond) = interval.upper.bound match {
        case Closed(v) => (v, matchVal <= v)
        case Open(v)   => (v - 1, matchVal < v)
        case Unbounded() =>
          throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      if (lower == upper) (matchVal === lower).anonymize
      else (lowerCond.anonymize && upperCond.anonymize).anonymize
    }
  }
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val interval: Interval[BigInt]
    }
    object Able {
      implicit class DFSIntPatternInt[R <: Int](val right: R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFULongPatternLong[R <: Long](val right: R)(implicit
          di: DummyImplicit
      ) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFSIntPatternBigInt[R <: BigInt](val right: R)
          extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(right)
      }
      implicit class DFSIntPatternRange[R <: Range](val right: R)
          extends Able[R] {
        val interval: Interval[BigInt] =
          Interval.fromRange(right).toBigIntInterval
      }
      implicit class DFSIntPatternIntervalInt[R <: Interval[Int]](val right: R)
          extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalLong[R <: Interval[Long]](
          val right: R
      ) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalBigInt[R <: Interval[BigInt]](
          val right: R
      ) extends Able[R] {
        val interval: Interval[BigInt] = right
      }
    }
    trait Builder[LType <: DFAny.Type]
        extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW]: Builder[Type[LW]] =
        new Builder[Type[LW]] {
          def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
            val reqInterval = IntervalSet(
              Interval.closed(
                BigInt.minSignedFromWidth(left.width),
                BigInt.maxSignedFromWidth(left.width)
              )
            )
            val patternSet = right
              .map(e => e.interval)
              .foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
                if (set.intersect(interval).nonEmpty)
                  throw new IllegalArgumentException(
                    s"\nThe interval $interval already intersects with $set"
                  )
                if (!reqInterval.contains(interval))
                  throw new IllegalArgumentException(
                    s"\nThe interval $interval is outside of range allowed: $reqInterval"
                  )
                set + interval
              })

            require(
              patternSet.intersect(reqInterval).nonEmpty,
              s"\nPattern must intersect with $reqInterval. Pattern is: $patternSet"
            )
            new Pattern(patternSet)
          }
        }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    class Able[L](val value: L) extends DFAny.Op.Able[L]
    class AbleOps[L](value: L) extends Able[L](value) {
      val left = value
      final def +[RW](right: DFSInt[RW])(implicit
          op: `Op+`.Builder[L, false, DFSInt[RW]]
      ) = op(left, right)
      final def +^[RW](right: DFSInt[RW])(implicit
          op: `Op+^`.Builder[L, true, DFSInt[RW]]
      ) = op(left, right)
      final def -[RW](right: DFSInt[RW])(implicit
          op: `Op-`.Builder[L, false, DFSInt[RW]]
      ) = op(left, right)
      final def -^[RW](right: DFSInt[RW])(implicit
          op: `Op-^`.Builder[L, true, DFSInt[RW]]
      ) = op(left, right)
      final def <[RW](right: DFSInt[RW])(implicit
          op: `Op<`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
      final def >[RW](right: DFSInt[RW])(implicit
          op: `Op>`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
      final def <=[RW](right: DFSInt[RW])(implicit
          op: `Op<=`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
      final def >=[RW](right: DFSInt[RW])(implicit
          op: `Op>=`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
      final def ===[RW](right: DFSInt[RW])(implicit
          op: `Op===`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
      final def =!=[RW](right: DFSInt[RW])(implicit
          op: `Op=!=`.Builder[L, DFSInt[RW]]
      ) = op(left, right)
    }
    trait Implicits {
      final implicit def __DFSIntWiden[FW, TW](c: DFSInt[FW])(implicit
          eq: OpContainer.Eq[FW, TW, Int]
      ): DFSInt[TW] = c.asInstanceOf[DFSInt[TW]]
      sealed class __DFSIntFromInt[L <: Int](left: L) extends AbleOps[L](left)
      final implicit def __DFSIntFromInt[L <: Int](
          left: L
      ): __DFSIntFromInt[L] = new __DFSIntFromInt(left)
      sealed class __DFSIntFromXInt[L <: XInt](left: L)
          extends AbleOps[ValueOf[L]](new ValueOf(left))
      final implicit def __DFSIntFromXInt[L <: XInt](
          left: L
      ): __DFSIntFromXInt[L] = new __DFSIntFromXInt(left)
      sealed class __DFSIntFromLong[L <: Long](left: L)(implicit
          di: DummyImplicit
      ) extends AbleOps[L](left)
      final implicit def __DFSIntFromLong[L <: Long](left: L)(implicit
          di: DummyImplicit
      ): __DFSIntFromLong[L] = new __DFSIntFromLong(left)
      sealed class __DFSIntFromXLong[L <: XLong](left: L)(implicit
          di: DummyImplicit
      ) extends AbleOps[ValueOf[L]](new ValueOf(left))
      final implicit def __DFSIntFromXLong[L <: XLong](left: L)(implicit
          di: DummyImplicit
      ): __DFSIntFromXLong[L] = new __DFSIntFromXLong(left)
      sealed class __DFSIntFromBigInt[L <: BigInt](left: L)
          extends AbleOps[L](left)
      final implicit def __DFSIntFromBigInt[L <: BigInt](
          left: L
      ): __DFSIntFromBigInt[L] = new __DFSIntFromBigInt[L](left)
      final implicit def __ofDFSInt[W](left: DFSInt[W]): Able[DFSInt[W]] =
        new Able(left)
      final implicit class __ExtendableDFSIntOps[LW](
          val left: DFSInt[LW] with Extendable
      ) {
        def +[R](right: Exact[R])(implicit
            op: `Op+`.Builder[DFSInt[LW], true, R]
        ) = op(left, right)
        def -[R](right: Exact[R])(implicit
            op: `Op-`.Builder[DFSInt[LW], true, R]
        ) = op(left, right)
      }
      final implicit class __DFSIntOps[LW](val left: DFSInt[LW]) {
        def maxValue: BigInt                          = BigInt(2) << (left.width - 1) - 1
        def sign(implicit ctx: DFAny.Context): DFBool = left.bit(left.width - 1)
        def unary_-(implicit op: `Op-`.Builder[0, true, DFSInt[LW]]) =
          op(0, left)
        def +[R](right: Exact[R])(implicit
            op: `Op+`.Builder[DFSInt[LW], false, R]
        ) = op(left, right)
        def +^[R](right: Exact[R])(implicit
            op: `Op+^`.Builder[DFSInt[LW], true, R]
        ) = op(left, right)
        def -[R](right: Exact[R])(implicit
            op: `Op-`.Builder[DFSInt[LW], false, R]
        ) = op(left, right)
        def -^[R](right: Exact[R])(implicit
            op: `Op-^`.Builder[DFSInt[LW], true, R]
        ) = op(left, right)
        def <[R](right: Exact[R])(implicit op: `Op<`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def >[R](right: Exact[R])(implicit op: `Op>`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def <=[R](right: Exact[R])(implicit op: `Op<=`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def >=[R](right: Exact[R])(implicit op: `Op>=`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def ===[R](right: Exact[R])(implicit
            op: `Op===`.Builder[DFSInt[LW], R]
        ) = op(left, right)
        def =!=[R](right: Exact[R])(implicit
            op: `Op=!=`.Builder[DFSInt[LW], R]
        ) = op(left, right)
        def <<[R](right: Exact[R])(implicit op: `Op<<`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def >>[R](right: Exact[R])(implicit op: `Op>>`.Builder[DFSInt[LW], R]) =
          op(left, right)
        def resize[RW](
            toWidth: SIntWidth.Checked[RW]
        )(implicit ctx: DFAny.Context): DFSInt[RW] =
          left.member match {
            case DFAny.Const(_, token: Token, _, _) =>
              DFAny.Const.forced[Type[RW]](token.resize(toWidth))
            case _ =>
              if (left.width.getValue == toWidth.getValue)
                left.asInstanceOf[DFSInt[RW]]
              else DFAny.Alias.Resize.sint(left.member, toWidth)
          }
        def extendable: DFSInt[LW] with Extendable =
          left.asInstanceOf[DFSInt[LW] with Extendable]
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
      final implicit def __DFSInt_ac_DFSInt[LW, RW](implicit
          ctx: DFAny.Context,
          checkLWvRW: `LW >= RW`.CheckedShell[LW, RW]
      ): Builder[Type[LW], DFSInt[RW]] =
        (left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          import DFDesign.Frontend._
          right.resize(left.width)
        }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op: Op)(
      func: (Token, Token) => DFBool.Token
  ) {
    @scala.annotation.implicitNotFound(
      "Dataflow variable ${L} does not support Comparison Ops with the type ${R}"
    )
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R] { type Out = DFBool }

    object Builder {
      object `VecW >= ConstW` extends Checked1Param.Int { //Needs to be mitigated to a warning
        type Cond[VW, CW] = VW >= CW
        type Msg[VW, CW] =
          "A static boolean result detected, due to an unsigned comparison between a DF variable and a larger number. Found: DFVar-width = " + ToString[
            VW
          ] + " and Num-width = " + ToString[CW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR: (L, R) => (DFSInt[LW], DFSInt[RW]))(
          implicit ctx: DFAny.Context
      ): Builder[L, R] =
        (leftL, rightR) => {
          val (left, right) = properLR(leftL, rightR)
          DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
        }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
          implicit
          ctx: DFAny.Context,
          checkLWvRW: `LW == RW`.CheckedShell[LW, RW]
      ): Builder[DFSInt[LW], DFSInt[RW]] =
        create[DFSInt[LW], LW, DFSInt[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](implicit
          ctx: DFAny.Context,
          rConst: DFAny.Const.AsIs.Aux[Type[LW], R, _ <: Type[RW]],
          checkLWvRW: `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ): Builder[DFSInt[LW], R] =
        create[DFSInt[LW], LW, R, RW]((left, rightNum) => {
          val right = rConst(left.dfType, rightNum).asInstanceOf[DFSInt[RW]]
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](implicit
          ctx: DFAny.Context,
          lConst: DFAny.Const.AsIs.Aux[Type[RW], L, _ <: Type[LW]],
          checkLWvRW: `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ): Builder[L, DFSInt[RW]] =
        create[L, LW, DFSInt[RW], RW]((leftNum, right) => {
          val left = lConst(right.dfType, leftNum).asInstanceOf[DFSInt[LW]]
          checkLWvRW.unsafeCheck(right.width, left.width)
          (left, right)
        })
    }
  }
  object `Op==`  extends OpsCompare(Func2.Op.==)(_ == _) with `Op==`
  object `Op!=`  extends OpsCompare(Func2.Op.!=)(_ != _) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)(_ == _)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)(_ != _)
  object `Op<`   extends OpsCompare(Func2.Op.<)(_ < _)
  object `Op>`   extends OpsCompare(Func2.Op.>)(_ > _)
  object `Op<=`  extends OpsCompare(Func2.Op.<=)(_ <= _)
  object `Op>=`  extends OpsCompare(Func2.Op.>=)(_ >= _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[Op <: Func2.Op.Negateable](val op: Op) {
    @scala.annotation.implicitNotFound(
      "Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}"
    )
    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[-L, LE, -R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = Max[LW, RW] + ITE[op.WC, 1, 0]
        type OutW[LW, RW, ResW] =
          TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Out
        def apply(
            properLR: (L, R) => (DFSInt[LW], DFSInt[RW])
        ): Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, OutW](implicit
            ctx: DFAny.Context,
            outW: Inference.OutW[LW, RW, OutW],
            doCheck: SafeBoolean[LE],
            checkLWvRW: `LW >= RW`.CheckedExtendable[LW, LE, RW]
        ): DetailedBuilder[L, LW, LE, R, RW] { type Out = DFSInt[OutW] } =
          new DetailedBuilder[L, LW, LE, R, RW] {
            type Out = DFSInt[OutW]
            def apply(
                properLR: (L, R) => (DFSInt[LW], DFSInt[RW])
            ): Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFSInt[OutW]
                def apply(leftL: L, rightR: R): Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  if (doCheck) checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val out     = Type(opWidth)
                  val func: (left.TToken, right.TToken) => out.TToken =
                    op match {
                      case _: Func2.Op.+  => _ + _
                      case _: Func2.Op.-  => _ - _
                      case _: Func2.Op.+^ => _ +^ _
                      case _: Func2.Op.-^ => _ -^ _
                      case _              => ???
                    }
                  DFAny.Func2(out, left, op, right)(func)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[LW, LE, RW](implicit
          detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFSInt_op_Const[LW, LE, R, RW](implicit
          ctx: DFAny.Context,
          rConst: DFAny.Const.AsIs.Aux[Type[LW], R, _ <: Type[RW]],
          detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, R, RW]
      ) =
        detailedBuilder((left, rightNum) => {
          val right = rConst(left.dfType, rightNum).asInstanceOf[DFSInt[RW]]
          (left, right)
        })

      implicit def evConst_op_DFSInt[L, LW, LE, RW](implicit
          ctx: DFAny.Context,
          lConst: DFAny.Const.AsIs.Aux[Type[RW], L, _ <: Type[LW]],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFSInt[RW], RW]
      ) =
        detailedBuilder((leftNum, right) => {
          (lConst(right.dfType, leftNum).asInstanceOf[DFSInt[LW]], right)
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
    def tokenFunc[LW](left: DFSInt.Token, right: DFUInt.Token): DFSInt.Token =
      left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW](left: DFSInt.Token, right: DFUInt.Token): DFSInt.Token =
      left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
