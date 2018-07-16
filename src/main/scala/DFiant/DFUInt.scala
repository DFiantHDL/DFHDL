package DFiant

import DFiant.basiclib._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import scodec.bits._
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

//    def within[Start, End](right : XRange[Start, End])(implicit op : OpWithin.Builder[TVal, XRange[Start, End]]) = op(left, right)

    def extBy[N](numOfBits : Natural.Int.Checked[N])(
      implicit tfs : TwoFace.Int.Shell2[+, LW, Int, N, Int]
    ) : DFUInt.Var[tfs.Out] = DFUInt.newVar(tfs(width, numOfBits), getInit).assign(left)

    def isZero = left == 0
    def isNonZero = left != 0
    //  def toDFSInt[SW](implicit tfs : TwoFace.Int.)
    def extendable : DFUInt[LW] with DFUInt.Extendable = DFUInt.extendable[LW](left)

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
  trait Var[W] extends DFUInt[W] with DFAny.Var {
    //Port Construction
    def <> [DIR <: DFDir](dir : DIR)(implicit port : Port.Builder[TVal, DIR]) : TVal <> DIR = port(this.asInstanceOf[TVal], dir)
  }

  trait Extendable {
    type Extendable = true
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit dsn : DFDesign, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit, n : NameIt
  ) : DFAny.NewVar with Var[W] = newVar(checkedWidth, Seq(DFUInt.Token(checkedWidth, 0)))
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit dsn : DFDesign, n : NameIt
  ) : DFAny.NewVar with Var[W] = newVar(checkedWidth.unsafeCheck(), Seq(DFUInt.Token(checkedWidth, 0)))
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
  final protected[DFiant] def newVar[W](width : TwoFace.Int[W], init : Seq[Token] = Seq())(
    implicit dsn : DFDesign, n : NameIt
  ) : DFAny.NewVar with Var[W] = new DFAny.NewVar(width, init) with Var[W] {
    def codeString(idRef : String) : String = s"val $idRef = DFUInt($width)"
  }

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[DFUInt.Token] = Seq())(implicit dsn : DFDesign, n : NameIt) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W] {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = {
        val bitsCodeString = if (relWidth == aliasedVar.width) "" else s"Unsupported DFUInt Alias codeString"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitsCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def extendable[W](extendedVar : DFUInt[W])(implicit dsn : DFDesign, n : NameIt) : Var[W] with Extendable =
    new DFAny.Alias(extendedVar, extendedVar.width, 0) with Var[W] with Extendable {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = s"$idRef.extendable"
      override def toString : String = s"DFUInt[$width] & Extendable"
    }

  protected[DFiant] def const[W](token : DFUInt.Token)(implicit dsn : DFDesign, n : NameIt) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def port[W, DIR <: DFDir](dfVar : DFUInt[W], dir : DIR)(implicit dsn : DFDesign, n : NameIt) : DFUInt[W] <> DIR =
    new DFAny.Port[DFUInt[W], DIR](dfVar, dir) with DFUInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val width : Int, val valueUInt : BigInt, val bubble : Boolean) extends DFAny.Token {
    lazy val valueBits : BitVector = valueUInt.toBitVector(width)
    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
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

  object Token {
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
  object Port extends Port {
    trait Builder[L <: DFAny, DIR <: DFDir] extends DFAny.Port.Builder[L, DIR]
    object Builder {
      implicit def conn[LW, DIR <: DFDir](implicit dsn : DFDesign, n : NameIt)
      : Builder[DFUInt[LW], DIR] = (right, dir) => port[LW, DIR](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
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
    trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
    object Builder {
      implicit def ev[LW](implicit dsn : DFDesign, n : NameIt) : Builder[DFUInt[LW]] = (left, right) =>  {
        DFUInt.alias(left, left.width, 0, 0, Able.toTokenSeq(left.width, right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends Prev {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[LW](implicit dsn : DFDesign, n : NameIt) : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
        def apply[P](left : DFUInt[LW], right : Natural.Int.Checked[P]) : DFUInt[LW] =
          DFUInt.alias(left, left.width, 0, -right, left.getInit)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def +  [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
      def -  [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
      def <  [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >  [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
      def <= [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >= [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def toDFUInt(implicit op : Const.PosOnly[Const.PosOnly[_,_],L]) = op(left)
    }
    trait Implicits extends super.Implicits {
      implicit class FromInt[L <: Int](left : L) extends Able[L](left)
      implicit class FromXInt[L <: XInt](left : L) extends Able[L](left)
      implicit class FromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      implicit class FromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      implicit class FromBigInt[L <: BigInt](left : L) extends Able[L](left)
      implicit def ofDFUInt[R <: DFUInt.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
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
      implicit def fromInt[N <: Int](implicit dsn : DFDesign, n : NameIt, w : BitsWidthOf.Int[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit dsn : DFDesign, n : NameIt, w : BitsWidthOf.Long[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit dsn : DFDesign, n : NameIt)
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
        dsn : DFDesign,
        n : NameIt,
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
        dsn : DFDesign,
        n : NameIt,
        checkPos : `N >= 0`.Long.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Long[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          const[W](Token(w(value), value))
        }
      }
      implicit def fromBigInt[Sym, N <: BigInt](implicit dsn : DFDesign, n : NameIt)
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
  // Assign
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=` extends `Op:=` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
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
        dsn : DFDesign, n : NameIt,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], DFUInt[RW], DFUInt[RW]] =
        create[DFUInt[LW], DFUInt[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        dsn : DFDesign, n : NameIt,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], R, DFUInt[RW]] = create[DFUInt[LW], R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right
      })
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) {
    //NCW = No-carry width
    //WCW = With-carry width
    class Component[NCW, WCW](val wc : DFUInt[WCW])(implicit dsn : DFDesign) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
      lazy val c = DFBits.alias[1](wc, 1, wc.width-1)
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = s"$idRef"
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
          dsn : DFDesign, n : NameIt,
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
                  import dsn.basicLib._
                  val (creationKind, left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = wcW(left.width, right.width)
                  val opInit = creationKind.opFunc(left.getInit, right.getInit)
                  val wc = newVar[WCW](opWidth, opInit)
                  creationKind match {
                    case `Ops+Or-`.+ =>
                      new `U+U`[LW, RW, WCW] {
                        val inLeft = ??? //FullyConnected(left)
                        val inRight = ??? //FullyConnected(right)
                        val outResult = ??? //FullyConnected(wc)
                      }
                    case `Ops+Or-`.- =>
                      new `U-U`[LW, RW, WCW] {
                        val inLeft = ??? //FullyConnected(left)
                        val inRight = ??? //FullyConnected(right)
                        val outResult = ??? //FullyConnected(wc)
                      }
                  }
                  // Creating extended component aliasing the op
                  new Component[NCW, WCW](wc).setAutoName(n.value)
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
        dsn : DFDesign, n : NameIt,
        rConst : Const.PosNeg.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val (right, negative) = rConst(rightNum)
        val creationKind = if (negative) -kind else kind
        (creationKind, left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        dsn : DFDesign, n : NameIt,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (kind, lConst(leftNum), right)
      })
    }
  }
  protected object `Ops+Or-` {
    abstract class Kind(val opString : String, val opFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFUInt.Token]) {
      def unary_- : Kind
    }
    case object + extends Kind("+", DFUInt.Token.+) {def unary_- : Kind = `Ops+Or-`.-}
    case object - extends Kind("-", DFUInt.Token.-) {def unary_- : Kind = `Ops+Or-`.+}
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
    class Component[NCW, WCW, CW](val wc : DFUInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(implicit dsn : DFDesign) extends DFAny.Alias(wc, ncW, 0) with DFUInt[NCW] {
      lazy val c = DFBits.alias[CW](wc, cW, wc.width - cW)
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = s"$idRef"
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
          dsn : DFDesign, n : NameIt,
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
                  import dsn.basicLib._
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val wcWidth = wcW(left.width, right.width)
                  val ncWidth = ncW(left.width, right.width)
                  val cWidth = cW(left.width, right.width)
                  val opInit = Token.*(left.getInit, right.getInit)
                  val wc = newVar[WCW](wcWidth, opInit)

                  new `U*U`[LW, RW, WCW] {
                    val inLeft = ??? //FullyConnected(left)
                    val inRight = ??? //FullyConnected(right)
                    val outResult = ??? //FullyConnected(wc)
                  }

                  // Creating extended component aliasing the op
                  new Component[NCW, WCW, CW](wc, ncWidth, cWidth).setAutoName(n.value)
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
        dsn : DFDesign, n : NameIt,
        rConst : Const.PosOnly.Aux[Builder[_,_,_], R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        dsn : DFDesign, n : NameIt,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[DiSoOpKind <: DiSoOp.Kind](opFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFBool.Token]) {
    type CompareOp[LW, RW] = basiclib.DiSoOp[DiSoOpKind, DFUInt[LW], DFUInt[RW], DFBool]
    def compareOp[LW, RW](inLeft : DFUInt[LW] <> IN, inRight : DFUInt[RW] <> IN, outResult : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW]

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

      def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW]))(implicit dsn : DFDesign, n : NameIt)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val result = DFBool.newVar(opFunc(left.getInit, right.getInit)).setAutoName(n.value)

//        compareOp[LW, RW] (
//          inLeft = ??? //FullyConnected(left),
//          inRight = ??? //FullyConnected(right),
//          outResult = ??? //FullyConnected(result)
//        )
        result
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        dsn : DFDesign, n : NameIt,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFUInt[LW], DFUInt[RW]] = create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        dsn : DFDesign, n : NameIt,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, R <: DFUInt[RW], RW](
        implicit
        dsn : DFDesign, n : NameIt,
        lConst : Const.PosOnly.Aux[Builder[_,_], L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare[DiSoOp.Kind.==](DFUInt.Token.==) with `Op==` {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U==U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op!=` extends OpsCompare[DiSoOp.Kind.!=](DFUInt.Token.!=) with `Op!=` {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U!=U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op<`  extends OpsCompare[DiSoOp.Kind.< ](DFUInt.Token.< ) {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U<U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op>`  extends OpsCompare[DiSoOp.Kind.> ](DFUInt.Token.> ) {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U>U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op<=` extends OpsCompare[DiSoOp.Kind.<=](DFUInt.Token.<=) {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U<=U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op>=` extends OpsCompare[DiSoOp.Kind.>=](DFUInt.Token.>=) {
    def compareOp[LW, RW](inLeft0 : DFUInt[LW] <> IN, inRight0 : DFUInt[RW] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[LW, RW] = {
      import dsn.basicLib._
      new `U>=U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}