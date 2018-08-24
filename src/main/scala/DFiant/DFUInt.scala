package DFiant

import DFiant.basiclib._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import shapeless.<:!<

trait DFUInt[W] extends DFUInt.Unbounded {
  type Width = W
}

object DFUInt extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFUInt.type] {
    type LW = Width
    type TVal = DFUInt[LW]
    type TVar = DFUInt.Var[LW]
    type TToken = DFUInt.Token
    type Extendable
    def +  [R](right: Op.Able[R])(implicit op: `Op+`.Builder[TVal, Extendable, R]) = op(left, right)
    def -  [R](right: Op.Able[R])(implicit op: `Op-`.Builder[TVal, Extendable, R]) = op(left, right)
    def *  [R](right: Op.Able[R])(implicit op: `Op*`.Builder[TVal, Extendable, R]) = op(left, right)
    //  def /  (right : DFUInt)         : DFUInt = ???

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
    ) : DFUInt[tfs.Out] = {
      val zeros = DFBits.const[LW](DFBits.Token(numOfBits, 0))
      new DFUInt.Alias[tfs.Out](List(zeros, this), AliasReference.AsIs(s".bits.uint")).setAutoConstructCodeString(s"$refCodeString.extendBy($numOfBits)")
    }

    def extendTo[EW](numOfBits : ExtWidth.Checked[EW, LW])(implicit ctx : DFAny.Alias.Context)
    : DFUInt[EW] = {
      val zeros = DFBits.const[LW](DFBits.Token(numOfBits, 0))
      new DFUInt.Alias[EW](List(zeros, this), AliasReference.AsIs(s".bits.uint")).setAutoConstructCodeString(s"$refCodeString.extendTo($numOfBits)")
    }

    def isZero(implicit ctx : DFAny.Op.Context) = left == 0
    def isNonZero(implicit ctx : DFAny.Op.Context) = left != 0
    def extendable(implicit ctx : DFAny.Alias.Context) : DFUInt[LW] with DFUInt.Extendable = DFUInt.extendable[LW](left)

    //    def within[Start, End](right : XRange[Start, End])(implicit op : OpWithin.Builder[TVal, XRange[Start, End]]) = op(left, right)
    //    trait matchdf extends super.matchdf {
    //      def casedf[R <: Unbounded](right : R)(block : => Unit)(implicit op: `Op==`.Builder[TVal, right.TVal]) : Unit = {}
    //      def casedf[R](that : Int)(block : => Unit)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : Unit = {}
    //      def casedf[R](that : Long)(block : => Unit)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : Unit = {}
    //      def casedf(that : BigInt)(block : => Unit)(implicit op: `Op==`.Builder[TVal, BigInt]) : Unit = {}
    //
    //    }
    override lazy val typeName: String = s"DFUInt[$width]"
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFUInt[W] with DFAny.Var {}

  trait Extendable {
    type Extendable = true
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit ctx : DFAny.NewVar.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : NewVar[W] = new NewVar[W](checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit ctx : DFAny.NewVar.Context
  ) : NewVar[W] = new NewVar[W](checkedWidth.unsafeCheck())
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
  final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar(width, s"DFUInt($width)") with Var[W] {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)

    //////////////////////////////////////////////////////////////////////////
    // Dataflow If
    //////////////////////////////////////////////////////////////////////////
    final object ifdf {
      import ConditionalBlock.WithRetVal._
      def apply[R](cond: DFBool)(block: => Op.Able[R])(
        implicit ctx : ConditionalBlock.WithRetVal.Context, op : `Op:=`.Builder[TVal, R]
      ) : DFIfBlock[TVal] = new DFIfBlock[TVal](cond, op(left, block).asInstanceOf[TVal], NewVar.this)(ctx, ctx.owner.mutableOwner)
    }
    //////////////////////////////////////////////////////////////////////////
  }

  final class Alias[W](aliasedVars : List[DFAny], reference : AliasReference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias(aliasedVars, reference) with Var[W] {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
  }

  protected[DFiant] def extendable[W](extendedVar : DFUInt[W])(implicit ctx : DFAny.Alias.Context)
  : Var[W] with Extendable = new DFAny.Alias(List(extendedVar), AliasReference.AsIs(".extendable")) with Var[W] with Extendable {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
    override def toString : String = s"DFUInt[$width] & Extendable"
  }

  protected[DFiant] def const[W](token : DFUInt.Token)(implicit ctx : DFAny.Const.Context) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def port[W, Dir <: DFDir](dfVar : DFUInt[W], dir : Dir)(implicit ctx : DFAny.Port.Context) : DFUInt[W] <> Dir =
    new DFAny.Port[DFUInt[W], Dir](dfVar, dir) with DFUInt[W] { }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val width : Int, val valueUInt : BigInt, val bubble : Boolean) extends DFAny.Token {
    lazy val valueBits : BitVector = valueUInt.toBitVector(width)
    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
    def toBubbleToken : Token = Token(width, Bubble)
    def mkTokenU(that : Token, result : BigInt, resultWidth : Int) : Token = {
      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
      else Token(resultWidth, result.asUnsigned(resultWidth))
    }

    final def + (that : Token) : Token = mkTokenU(that, this.valueUInt + that.valueUInt, scala.math.max(this.width, that.width) + 1)
    final def - (that : Token) : Token = mkTokenU(that, this.valueUInt - that.valueUInt, scala.math.max(this.width, that.width) + 1)
    final def * (that : Token) : Token = mkTokenU(that, this.valueUInt * that.valueUInt, this.width + that.width)
    final def / (that : Token) : Token = mkTokenU(that, this.valueUInt / that.valueUInt, this.width)
    final def % (that : Token) : Token = mkTokenU(that, this.valueUInt % that.valueUInt, that.width)
    final def <  (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt < that.valueUInt, this.isBubble || that.isBubble)
    final def >  (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt > that.valueUInt, this.isBubble || that.isBubble)
    final def <= (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt <= that.valueUInt, this.isBubble || that.isBubble)
    final def >= (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt >= that.valueUInt, this.isBubble || that.isBubble)
    final def == (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt == that.valueUInt, this.isBubble || that.isBubble)
    final def != (that : Token) : DFBool.Token = DFBool.Token(this.valueUInt != that.valueUInt, this.isBubble || that.isBubble)

    override def codeString: String = if (isBubble) "Φ" else valueUInt.codeString
    override def valueString : String = valueUInt.toString()
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
    def apply(width : Int, value : BigInt) : Token = {
      if (value < 0 ) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      new Token(width, value, false)
    }
    def apply(width : Int, value : Bubble) : Token = new Token(width, 0, true)
    def apply(width : Int, token : Token) : Token = {
      //TODO: Boundary checks
      new Token(width, token.valueUInt, token.bubble)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[LW, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFUInt[LW], Dir] = (right, dir) => port[LW, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.Width](List(left), AliasReference.AsIs(s".as(DFUInt(${mold.width}))"))
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

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFUInt[LW]]]) : Seq[Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFUInt.Token(width, t)
          case (t : DFUInt.Token) => DFUInt.Token(width, t)
          case (t : Int) => DFUInt.Token(width, t)
          case (t : Long) => DFUInt.Token(width, t)
          case (t : BigInt) => DFUInt.Token(width, t)
        })

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFUInt[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends PrevCO {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[LW](implicit ctx : DFAny.Alias.Context) : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
        def apply[P](left : DFUInt[LW], right : Natural.Int.Checked[P]) : DFUInt[LW] =
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
      def +  [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
      def -  [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
      def <  [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >  [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
      def <= [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >= [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def <> [RW, RDIR <: DFDir](port : DFUInt[RW] <> RDIR)(
        implicit op: `Op<>`.Builder[DFUInt[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
      def toDFUInt(implicit op : Const.PosOnly[Const.PosOnly[_,_],L]) = op(left)
    }
    trait Implicits {
      sealed class DFUIntFromInt[L <: Int](left : L) extends Able[L](left)
      final implicit def DFUIntFromInt[L <: Int](left: L): DFUIntFromInt[L] = new DFUIntFromInt(left)
      sealed class DFUIntFromXInt[L <: XInt](left : L) extends Able[L](left)
      final implicit def DFUIntFromXInt[L <: XInt](left: L): DFUIntFromXInt[L] = new DFUIntFromXInt(left)
      sealed class DFUIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFUIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFUIntFromLong[L] = new DFUIntFromLong(left)
      sealed class DFUIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFUIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFUIntFromXLong[L] = new DFUIntFromXLong(left)
      sealed class DFUIntFromBigInt[L <: BigInt](left : L) extends Able[L](left)
      final implicit def DFUIntFromBigInt[L <: BigInt](left: L): DFUIntFromBigInt[L] = new DFUIntFromBigInt[L](left)
      final implicit def ofDFUInt[R <: DFUInt.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFUInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait PosNeg[N] {
      type W
      def apply(value : N) : (DFUInt[W], Boolean)
    }
    object PosNeg {
      type Aux[N, W0] = PosNeg[N]{type W = W0}
      import singleton.ops.math.Abs
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Int[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Long[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[N, Int] = new PosNeg[N] {
        type W = Int
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = value.abs
          (const[W](Token(absValue.bitsWidth, absValue)), value < 0)
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
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Int.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Int[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          const[W](Token(w(value), value))
        }
      }
      implicit def fromLong[Sym, N <: Long](
        implicit
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Long.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Long[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          const[W](Token(w(value), value))
        }
      }
      implicit def fromBigInt[Sym, N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[Sym, N, Int] = new PosOnly[Sym, N] {
        type W = Int
        def apply(value : N) : DFUInt[W] = {
          `N >= 0`.BigInt.unsafeCheck(value)
          const[W](Token(value.bitsWidth, value))
        }
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

      def create[L, R, RW](properR : (L, R) => DFUInt[RW]) : Aux[L, R, DFUInt[RW]] =
        new Builder[L, R] {
          type Comp = DFUInt[RW]
          def apply(leftL : L, rightR : R) : Comp =  properR(leftL, rightR)
        }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : Ctx,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], DFUInt[RW], DFUInt[RW]] =
        create[DFUInt[LW], DFUInt[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx : Ctx,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], R, DFUInt[RW]] = create[DFUInt[LW], R, RW]((left, rightNum) => {
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
  protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) {
    //NCW = No-carry width
    //WCW = With-carry width
    class Component[NCW, WCW](val wc : DFUInt[WCW])(implicit ctx : DFAny.Alias.Context) extends
      DFAny.Alias(List(wc), AliasReference.BitsWL(wc.width-1, 0, s".bits(${wc.width-2}, 0).uint")) with DFUInt[NCW] {
      lazy val c = new DFBool.Alias(List(wc), AliasReference.BitsWL(1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx.getName}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = Max[LW, RW] + 1
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW]
            def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFUIntOps._
                  val (creationKind, left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = wcW(left.width, right.width)
                  val opInst = creationKind match {
                    case `Ops+Or-`.+ => new DFiant.basiclib.DFUIntOps.`Comp+`(left.width, right.width, opWidth)
                    case `Ops+Or-`.- => new DFiant.basiclib.DFUIntOps.`Comp-`(left.width, right.width, opWidth)
                  }
                  opInst.setAutoName(s"${ctx.getName}Comp")
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFUInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.getName}WC")
                  // Creating extended component aliasing the op
                  new Component[NCW, WCW](wc)
                }
              }
          }
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((left, right) => (kind, left, right))

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosNeg.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val (right, negative) = rConst(rightNum)
        val creationKind = if (negative) -kind else kind
        (creationKind, left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (kind, lConst(leftNum), right)
      })
    }
  }
  protected object `Ops+Or-` {
    abstract class Kind(val opString : String) {
      def unary_- : Kind
    }
    case object + extends Kind("+") {def unary_- : Kind = `Ops+Or-`.-}
    case object - extends Kind("-") {def unary_- : Kind = `Ops+Or-`.+}
  }
  object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
  object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // * operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op*` {
    //NCW = No-carry width
    //WCW = With-carry width
    //CW = Carry width
    class Component[NCW, WCW, CW](val wc : DFUInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(
      implicit ctx : DFAny.Alias.Context
    ) extends DFAny.Alias(List(wc), AliasReference.BitsWL(ncW, 0, s".bits(${wc.width-cW-1}, 0).uint")) with DFUInt[NCW] {
      lazy val c = new DFBits.Alias[CW](List(wc), AliasReference.BitsWL(cW, wc.width - cW, s".bits(${wc.width-1}, ${wc.width-cW})")).setAutoName(s"${ctx.getName}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Op `*` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
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

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, CW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          cW : Inference.CW[LW, RW, CW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW, CW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW, CW]
            def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW, CW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFUIntOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val wcWidth = wcW(left.width, right.width)
                  val ncWidth = ncW(left.width, right.width)
                  val cWidth = cW(left.width, right.width)

                  val opInst = new DFiant.basiclib.DFUIntOps.`Comp*`(left.width, right.width, wcWidth)
                  opInst.setAutoName(s"${ctx.getName}Comp")
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFUInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.getName}WC")

                  // Creating extended component aliasing the op
                  new Component[NCW, WCW, CW](wc, ncWidth, cWidth)
                }
              }
          }
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_,_], R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFBool.Token]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

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

      def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import ctx.basicLib.DFUIntOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => new DFiant.basiclib.DFUIntOps.`Comp==`(left.width, right.width)
          case DiSoOp.Kind.!= => new DFiant.basiclib.DFUIntOps.`Comp!=`(left.width, right.width)
          case DiSoOp.Kind.<  => new DFiant.basiclib.DFUIntOps.`Comp<`(left.width, right.width)
          case DiSoOp.Kind.>  => new DFiant.basiclib.DFUIntOps.`Comp>`(left.width, right.width)
          case DiSoOp.Kind.<= => new DFiant.basiclib.DFUIntOps.`Comp<=`(left.width, right.width)
          case DiSoOp.Kind.>= => new DFiant.basiclib.DFUIntOps.`Comp>=`(left.width, right.width)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.setAutoName(s"${ctx.getName}Comp")
        opInst.inLeft <> left
        opInst.inRight <> right
        opInst.outResult
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFUInt[LW], DFUInt[RW]] = create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_], L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFUInt.Token.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFUInt.Token.!=) with `Op!=`
  object `Op<`  extends OpsCompare(DiSoOp.Kind.< )(DFUInt.Token.< )
  object `Op>`  extends OpsCompare(DiSoOp.Kind.> )(DFUInt.Token.> )
  object `Op<=` extends OpsCompare(DiSoOp.Kind.<=)(DFUInt.Token.<=)
  object `Op>=` extends OpsCompare(DiSoOp.Kind.>=)(DFUInt.Token.>=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}