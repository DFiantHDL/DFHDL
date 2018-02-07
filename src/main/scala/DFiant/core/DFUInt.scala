package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import scodec.bits._

trait DFUInt[W] extends DFAny.Val[W, DFUInt.type, DFUInt[W], DFUInt.Var[W]] {
  left =>
  import DFUInt.Operations._
  type Extendable
  def +[R](right: TAble[R])(implicit op: `Op+`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def -[R](right: TAble[R])(implicit op: `Op-`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def *[R](right: TAble[R])(implicit op: `Op*`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
//  def /  (right : DFUInt)         : DFUInt = ???

  def <  [R](right: TAble[R])(implicit op: `Op<`.Builder[DFUInt[W], R]) = op(left, right)
  def >  [R](right: TAble[R])(implicit op: `Op>`.Builder[DFUInt[W], R]) = op(left, right)
  def <= [R](right: TAble[R])(implicit op: `Op<=`.Builder[DFUInt[W], R]) = op(left, right)
  def >= [R](right: TAble[R])(implicit op: `Op>=`.Builder[DFUInt[W], R]) = op(left, right)

  def == [RW](right : DFUInt[RW])(implicit op: `Op==`.Builder[DFUInt[W], DFUInt[RW]]) = op(left, right)
  def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[DFUInt[W], R]) = op(left, right)
  def == [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[DFUInt[W], R]) = op(left, right)
  def == (that : BigInt)(implicit op: `Op==`.Builder[DFUInt[W], BigInt]) = op(left, that)
  def != [RW](right : DFUInt[RW])(implicit op: `Op!=`.Builder[DFUInt[W], DFUInt[RW]]) = op(left, right)
  def != [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[DFUInt[W], R]) = op(left, right)
  def != [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[DFUInt[W], R]) = op(left, right)
  def != (that : BigInt)(implicit op: `Op!=`.Builder[DFUInt[W], BigInt]) = op(left, that)

  def extBy[N](numOfBits : Natural.Int.Checked[N])(
    implicit dsn : DFDesign, tfs : TwoFace.Int.Shell2[+, W, Int, N, Int]
  ) : DFUInt.Var[tfs.Out] = DFUInt.newVar(tfs(width, numOfBits)).init(getInit).assign(left)

  override def toString : String = s"DFUInt[$width]"

  def isZero(implicit dsn : DFDesign) = left == 0
  def isNonZero(implicit dsn : DFDesign) = left != 0
//  def toDFSInt[SW](implicit tfs : TwoFace.Int.)
  def extendable(implicit dsn : DFDesign) : DFUInt[W] with DFUInt.Extendable = DFUInt.extendable[W](this)
}


object DFUInt extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFUInt[W] with DFAny.Var[W, DFUInt.type, DFUInt[W], DFUInt.Var[W]] {
    left =>
    import DFUInt.Operations._
    def := [R](right: TAble[R])(implicit op: `Op:=`.Builder[DFUInt[W], R]) = op(left, right)
  }

  trait Extendable {
    type Extendable = true
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit dsn : DFDesign, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : Var[W] = newVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit dsn : DFDesign
  ) : Var[W] = newVar(checkedWidth.unsafeCheck())
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
  protected[DFiant] def newVar[W](width : TwoFace.Int[W])(implicit dsn : DFDesign) : Var[W] =
    new DFAny.NewVar(width, Seq(DFUInt.Token(width, 0))) with Var[W] {
      def codeString(idRef : String) : String = s"val $idRef = DFUInt($width)"
    }

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[DFUInt.Token] = Seq())(implicit dsn : DFDesign) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W] {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = {
        val bitsCodeString = if (relWidth == aliasedVar.width) "" else s"Unsupported DFUInt Alias codeString"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitsCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def extendable[W](extendedVar : DFUInt[W])(implicit dsn : DFDesign) : Var[W] with Extendable =
    new DFAny.Alias(extendedVar, extendedVar.width, 0) with Var[W] with Extendable {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      def codeString(idRef : String) : String = s"$idRef.extendable"
      override def toString : String = s"DFUInt[$width] & Extendable"
    }

  protected[DFiant] def const[W](token : DFUInt.Token)(implicit dsn : DFDesign) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[DFUInt.Token], args : DFAny*)(implicit dsn : DFDesign) : DFUInt[W] =
    new DFAny.Op(width, opString, opInit, args) with DFUInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val width : Int, val valueUInt : BigInt, val bubble : Boolean) extends DFAny.Token {
    lazy val valueBits : BitVector = BitVector(valueUInt.toByteArray).toLength(width)
    lazy val bubbleMask: BitVector = BitVector.fill(width)(bubble)
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
    def +  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l,r) => l + r)
    def -  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l,r) => l - r)
    def *  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l,r) => l * r)
    def /  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l,r) => l / r)
    def %  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l,r) => l % r)
    def <  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l < r)
    def >  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l > r)
    def <= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l <= r)
    def >= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l >= r)
    def == (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l == r)
    def != (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l,r) => l != r)
    //  def unary_- (left : Token) : Token = -left

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

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFUInt[LW]]]) : Seq[DFUInt.Token] =
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
      implicit def ev[LW](implicit dsn : DFDesign) : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
        def apply(left : DFUInt[LW], right : Seq[Able[DFUInt[LW]]]) : DFUInt[LW] =
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
      implicit def ev[LW](implicit dsn : DFDesign) : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
        def apply[P](left : DFUInt[LW], right : Natural.Int.Checked[P]) : DFUInt[LW] =
          DFUInt.alias(left, left.width, 0, -right, left.getInit)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Able
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Able[R](value : R) extends DFAny.Able[R](value)
  object Able {
    implicit class OfInt[R <: Int](value : R) extends Able[R](value)
    implicit class OfXInt[R <: XInt](value : R) extends Able[R](value)
    implicit class OfLong[R <: Long](value : R)(implicit di : DummyImplicit) extends Able[R](value)
    implicit class OfXLong[R <: XLong](value : R)(implicit di : DummyImplicit) extends Able[R](value)
    implicit class OfBigInt[R <: BigInt](value : R) extends Able[R](value)
    implicit class OfDFUInt[RW](value : DFUInt[RW]) extends Able[DFUInt[RW]](value)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Assign extends Assign {
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicit Construction of LHS
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Implicits {
    implicit class FromInt[L <: Int](left : L) extends Operations[L](left)
    implicit class FromXInt[L <: XInt](left : L) extends Operations[L](left)
    implicit class FromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Operations[L](left)
    implicit class FromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Operations[L](left)
    implicit class FromBigInt[L <: BigInt](left : L) extends Operations[L](left)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Operations[L](left : L) {
    import Operations._
    def +  [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def -  [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def <  [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
    def >  [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
    def <= [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
    def >= [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
    def toDFUInt(implicit op : `Op.toDFUInt`.Builder[L]) = op(left)
  }

  object Operations {
    trait General {
      trait BuilderTop[L, R] {
        type Comp
        def apply(left : L, rightR : R) : Comp
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // +/- operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) extends General {
      //NCW = No-carry width
      //WCW = With-carry width
      class Component[NCW, WCW](val wc : DFUInt[WCW])(implicit dsn : DFDesign) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        lazy val c = DFBits.alias[1](wc, 1, wc.width-1)
        protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
        def codeString(idRef : String) : String = s"$idRef"
      }

      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
      trait Builder[L, LE, R] extends BuilderTop[L, R]

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

        object `L >= 0` extends `N >= 0` {
          type MsgCommon[L] = "Operation does not permit a negative number on the LHS. Found literal: " + ToString[L]
        }

        object Inference {
          import singleton.ops.math.Max
          type CalcWCW[LW, RW] = Max[LW, RW] + 1
          type WCW[LW, RW] = TwoFace.Int.Shell2[CalcWCW, LW, Int, RW, Int]
          type CalcNCW[LW, RW] = Max[LW, RW]
          type NCW[LW, RW] = TwoFace.Int.Shell2[CalcNCW, LW, Int, RW, Int]
        }

        trait DetailedBuilder[L, LW, LE, R, RW] {
          type Comp
          def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
        }
        object DetailedBuilder {
          implicit def ev[L, LW, LE, R, RW](
            implicit
            dsn : DFDesign,
            ncW : Inference.NCW[LW, RW],
            wcW : Inference.WCW[LW, RW],
            checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
          ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[ncW.Out, wcW.Out]} =
            new DetailedBuilder[L, LW, LE, R, RW]{
              type Comp = Component[ncW.Out, wcW.Out]
              def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
                new Builder[L, LE, R] {
                  type Comp = Component[ncW.Out, wcW.Out]
                  def apply(leftL : L, rightR : R) : Comp = {
                    val (creationKind, left, right) = properLR(leftL, rightR)
                    // Completing runtime checks
                    checkLWvRW.unsafeCheck(left.width, right.width)
                    // Constructing op
                    val opWidth = wcW(left.width, right.width)
                    val opInit = creationKind.opFunc(left.getInit, right.getInit)
                    val wc = new DFAny.Op(opWidth, creationKind.opString, opInit, Seq(left, right)) with DFUInt[wcW.Out] {
                      override def refCodeString(idRef : String) : String = s"$idRef.wc"
                    }
                    // Creating extended component aliasing the op
                    new Component[ncW.Out, wcW.Out](wc)
                  }
                }
            }
        }

        import singleton.ops.math.Abs
        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
          implicit
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((left, right) => (kind, left, right))

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, LE, R <: Int, RW](
          implicit
          dsn : DFDesign,
          rW : BitsWidthOf.IntAux[Abs[R], RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[RW](DFUInt.Token(rW(right), right)))
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, LE, R <: Long, RW](
          implicit
          dsn : DFDesign,
          rW : BitsWidthOf.LongAux[Abs[R], RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[RW](DFUInt.Token(rW(right), right)))
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW, LE](
          implicit
          dsn : DFDesign,
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, BigInt, Int]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[Int](DFUInt.Token(right.bitsWidth, right)))
        })

        implicit def evInt_op_DFUInt[L <: Int, LW, LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          lCheck : `L >= 0`.Int.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.IntAux[Abs[L], LW],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          lCheck.unsafeCheck(leftNum)
          (kind, DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum)), right)
        })

        implicit def evLong_op_DFUInt[L <: Long, LW, LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          lCheck : `L >= 0`.Long.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.LongAux[Abs[L], LW],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          lCheck.unsafeCheck(leftNum)
          (kind, DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum)), right)
        })

        implicit def evBigInt_op_DFUInt[LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          detailedBuilder: DetailedBuilder[BigInt, Int, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          `L >= 0`.BigInt.unsafeCheck(leftNum)
          (kind, DFUInt.const[Int](DFUInt.Token(leftNum.bitsWidth, leftNum)), right)
        })
      }
    }
    protected object `Ops+Or-` {
      abstract class Kind(val opString : String, val opFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFUInt.Token]) {
        def unary_- : Kind
      }
      case object + extends Kind("+", DFUInt.Token.+) {
        def unary_- : Kind = `Ops+Or-`.-
      }
      case object - extends Kind("-", DFUInt.Token.-) {
        def unary_- : Kind = `Ops+Or-`.+
      }
    }
    object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
    object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // * operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object `Op*` extends General {
      //NCW = No-carry width
      //WCW = With-carry width
      //CW = Carry width
      class Component[NCW, WCW, CW](val wc : DFUInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(implicit dsn : DFDesign) extends DFAny.Alias(wc, ncW, 0) with DFUInt[NCW] {
        lazy val c = DFBits.alias[CW](wc, cW, wc.width - cW)
        protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
        def codeString(idRef : String) : String = s"$idRef"
      }

      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Op `*` with the type ${R}")
      trait Builder[L, LE, R] extends BuilderTop[L, R]

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

        object `N >= 0` extends `N >= 0` {
          type MsgCommon[N] = "Operation does not permit negative numbers. Found literal: " + ToString[N]
        }

        object Inference {
          import singleton.ops.math.Max
          type CalcWCW[LW, RW] = LW + RW
          type WCW[LW, RW] = TwoFace.Int.Shell2[CalcWCW, LW, Int, RW, Int]
          type CalcNCW[LW, RW] = Max[LW, RW]
          type NCW[LW, RW] = TwoFace.Int.Shell2[CalcNCW, LW, Int, RW, Int]
          type CalcCW[LW, RW] = CalcWCW[LW, RW] - CalcNCW[LW, RW]
          type CW[LW, RW] = TwoFace.Int.Shell2[CalcCW, LW, Int, RW, Int]
        }

        trait DetailedBuilder[L, LW, LE, R, RW] {
          type Comp
          def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
        }
        object DetailedBuilder {
          implicit def ev[L, LW, LE, R, RW](
            implicit
            dsn : DFDesign,
            ncW : Inference.NCW[LW, RW],
            wcW : Inference.WCW[LW, RW],
            cW : Inference.CW[LW, RW],
            checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
          ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[ncW.Out, wcW.Out, cW.Out]} =
            new DetailedBuilder[L, LW, LE, R, RW]{
              type Comp = Component[ncW.Out, wcW.Out, cW.Out]
              def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
                new Builder[L, LE, R] {
                  type Comp = Component[ncW.Out, wcW.Out, cW.Out]
                  def apply(leftL : L, rightR : R) : Comp = {
                    val (left, right) = properLR(leftL, rightR)
                    // Completing runtime checks
                    checkLWvRW.unsafeCheck(left.width, right.width)
                    // Constructing op
                    val wcWidth = wcW(left.width, right.width)
                    val ncWidth = ncW(left.width, right.width)
                    val cWidth = cW(left.width, right.width)
                    val opInit = Token.*(left.getInit, right.getInit)
                    val wc = new DFAny.Op(wcWidth, "*", opInit, Seq(left, right)) with DFUInt[wcW.Out] {
                      override def refCodeString(idRef : String) : String = s"$idRef.wc"
                    }
                    // Creating extended component aliasing the op
                    new Component[ncW.Out, wcW.Out, cW.Out](wc, ncWidth, cWidth)
                  }
                }
            }
        }

        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
          implicit
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((left, right) => (left, right))

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, LE, R <: Int, RW](
          implicit
          dsn : DFDesign,
          rCheck : `N >= 0`.Int.CheckedShellSym[Builder[_,_,_], L],
          rW : BitsWidthOf.IntAux[R, RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          rCheck.unsafeCheck(rightNum)
          (left, DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum)))
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, LE, R <: Long, RW](
          implicit
          dsn : DFDesign,
          rCheck : `N >= 0`.Long.CheckedShellSym[Builder[_,_,_], L],
          rW : BitsWidthOf.LongAux[R, RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          rCheck.unsafeCheck(rightNum)
          (left, DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum)))
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW, LE](
          implicit
          dsn : DFDesign,
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, BigInt, Int]
        ) = detailedBuilder((left, rightNum) => {
          `N >= 0`.BigInt.unsafeCheck(rightNum)
          (left, DFUInt.const[Int](DFUInt.Token(rightNum.bitsWidth, rightNum)))
        })

        implicit def evInt_op_DFUInt[L <: Int, LW, LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          lCheck : `N >= 0`.Int.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.IntAux[L, LW],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          lCheck.unsafeCheck(leftNum)
          (DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum)), right)
        })

        implicit def evLong_op_DFUInt[L <: Long, LW, LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          lCheck : `N >= 0`.Long.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.LongAux[L, LW],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          lCheck.unsafeCheck(leftNum)
          (DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum)), right)
        })

        implicit def evBigInt_op_DFUInt[LE, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          detailedBuilder: DetailedBuilder[BigInt, Int, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          `N >= 0`.BigInt.unsafeCheck(leftNum)
          (DFUInt.const[Int](DFUInt.Token(leftNum.bitsWidth, leftNum)), right)
        })
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Comparison operations
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected abstract class OpsCompare(kind : OpsCompare.Kind) extends General {
      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
      trait Builder[L, R] extends BuilderTop[L, R]

      object Builder {
        type Aux[L, R, Comp0] = Builder[L, R] {
          type Comp = Comp0
        }

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

        object `N >= 0` extends `N >= 0`{
          type MsgCommon[R] = "Unsigned comparison operations do not permit negative numbers. Found: " + ToString[R]
        }

        def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW]))(
          implicit dsn : DFDesign
        ) : Aux[L, R, DFBool] =
          new Builder[L, R] {
          type Comp = DFBool
          def apply(leftL : L, rightR : R) : Comp = {
            val (left, right) = properLR(leftL, rightR)
            DFBool.op(kind.opString, kind.opFunc(left.getInit, right.getInit), left, right)
          }
        }

        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
        ) : Aux[DFUInt[LW], DFUInt[RW], DFBool] =
          create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
            checkLWvRW.unsafeCheck(left.width, right.width)
            (left, right)
          })

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, R <: Int, RW](
          implicit
          dsn : DFDesign,
          checkR : `N >= 0`.Int.CheckedShellSym[Builder[_,_], R],
          rW : BitsWidthOf.IntAux[R, RW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
        ) : Aux[DFUInt[LW], R, DFBool] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, R <: Long, RW](
          implicit
          dsn : DFDesign,
          checkR : `N >= 0`.Long.CheckedShellSym[Builder[_,_], R],
          rW : BitsWidthOf.LongAux[R, RW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
        ) : Aux[DFUInt[LW], R, DFBool] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW](
          implicit
          dsn : DFDesign,
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, Int]
        ) : Aux[DFUInt[LW], BigInt, DFBool] = create[DFUInt[LW], LW, BigInt, Int]((left, rightNum) => {
          `N >= 0`.BigInt.unsafeCheck(rightNum)
          val right = DFUInt.const[Int](DFUInt.Token(rightNum.bitsWidth, rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evInt_op_DFUInt[L <: Int, LW, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          checkL : `N >= 0`.Int.CheckedShellSym[Builder[_,_], L],
          lW : BitsWidthOf.IntAux[L, LW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
        ) : Aux[L, DFUInt[RW], DFBool] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
          checkL.unsafeCheck(leftNum)
          val left = DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum))
          checkLWvRW.unsafeCheck(right.width, left.width)
          (left, right)
        })

        implicit def evLong_op_DFUInt[L <: Long, LW, R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          checkL : `N >= 0`.Long.CheckedShellSym[Builder[_,_], L],
          lW : BitsWidthOf.LongAux[L, LW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
        ) : Aux[L, DFUInt[RW], DFBool] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
          checkL.unsafeCheck(leftNum)
          val left = DFUInt.const[LW](DFUInt.Token(lW(leftNum), leftNum))
          checkLWvRW.unsafeCheck(right.width, left.width)
          (left, right)
        })

        implicit def evBigInt_op_DFUInt[R <: DFUInt[RW], RW](
          implicit
          dsn : DFDesign,
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, Int]
        ) : Aux[BigInt, DFUInt[RW], DFBool] = create[BigInt, Int, DFUInt[RW], RW]((leftNum, right) => {
          `N >= 0`.BigInt.unsafeCheck(leftNum)
          val left = DFUInt.const[Int](DFUInt.Token(leftNum.bitsWidth, leftNum))
          checkLWvRW.unsafeCheck(right.width, left.width)
          (left, right)
        })
      }
    }
    protected object OpsCompare {
      class Kind(val opString : String, val opFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFBool.Token])
      case object == extends Kind("==", DFUInt.Token.==)
      case object != extends Kind("!=", DFUInt.Token.!=)
      case object <  extends Kind("<", DFUInt.Token.<)
      case object >  extends Kind(">", DFUInt.Token.>)
      case object <= extends Kind("<=", DFUInt.Token.<=)
      case object >= extends Kind(">=", DFUInt.Token.>=)
    }
    object `Op==` extends OpsCompare(OpsCompare.==)
    object `Op!=` extends OpsCompare(OpsCompare.!=)
    object `Op<`  extends OpsCompare(OpsCompare.<)
    object `Op>`  extends OpsCompare(OpsCompare.>)
    object `Op<=` extends OpsCompare(OpsCompare.<=)
    object `Op>=` extends OpsCompare(OpsCompare.>=)
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Assignment := operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object `Op:=` extends General {
      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment operation with the type ${R}")
      trait Builder[L, R] extends BuilderTop[L, R]

      object Builder {
        type Aux[L, R, Comp0] = Builder[L, R] {
          type Comp = Comp0
        }

        object `LW >= RW` extends Checked1Param.Int {
          type Cond[LW, RW] = LW >= RW
          type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
          type ParamFace = Int
        }

        object `R >= 0` extends `N >= 0` {
          type MsgCommon[R] = "An assignment operation does not permit negative numbers. Found: " + ToString[R]
        }

        def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Aux[L, R, DFUInt.Var[LW]] =
          new Builder[L, R] {
            type Comp = DFUInt.Var[LW]
            def apply(leftL : L, rightR : R) : Comp = {
              val (left, right) = properLR(leftL, rightR)
              left.assign(right)
            }
          }

        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
          implicit
          checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
        ) : Aux[DFUInt[LW], DFUInt[RW], DFUInt.Var[LW]] =
          create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
            checkLWvRW.unsafeCheck(left.width, right.width)
            (left, right)
          })

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, R <: Int, RW](
          implicit
          dsn : DFDesign,
          checkR : `R >= 0`.Int.CheckedShellSym[Builder[_,_], R],
          rW : BitsWidthOf.IntAux[R, RW],
          checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
        ) : Aux[DFUInt[LW], R, DFUInt.Var[LW]] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, R <: Long, RW](
          implicit
          dsn : DFDesign,
          checkR : `R >= 0`.Long.CheckedShellSym[Builder[_,_], R],
          rW : BitsWidthOf.LongAux[R, RW],
          checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
        ) : Aux[DFUInt[LW], R, DFUInt.Var[LW]] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](DFUInt.Token(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW](
          implicit
          dsn : DFDesign,
          checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, Int]
        ) : Aux[DFUInt[LW], BigInt, DFUInt.Var[LW]] = create[DFUInt[LW], LW, BigInt, Int]((left, rightNum) => {
          `R >= 0`.BigInt.unsafeCheck(rightNum)
          val right = DFUInt.const[Int](DFUInt.Token(rightNum.bitsWidth, rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Conversion operation toDFUInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object `Op.toDFUInt` {
      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support conversion operation toDFUInt")
      trait Builder[L] {
        type Comp
        def apply(leftL : L) : Comp
      }

      object Builder {
        type Aux[L, Comp0] = Builder[L] {
          type Comp = Comp0
        }

        private object `N >= 0` extends `N >= 0` {
          type MsgCommon[N] = "Conversion from number to DFUInt does not permit negative numbers. Found: " + ToString[N]
        }

        def createConst[L, LW](properL : L => DFUInt.Token)(
          implicit dsn : DFDesign
        ) : Aux[L, DFUInt[LW]] =
          new Builder[L] {
            type Comp = DFUInt[LW]
            def apply(leftL : L) : Comp = {
              val left = properL(leftL)
              DFUInt.const[LW](left)
            }
          }

        implicit def evInt[L <: Int, LW](
          implicit
          dsn : DFDesign,
          lCheck : `N >= 0`.Int.CheckedShell[L],
          w : BitsWidthOf.IntAux[L, LW]
        ) : Aux[L, DFUInt[LW]] = createConst[L, LW](left => {
          lCheck.unsafeCheck(left)
          DFUInt.Token(w(left), left)
        })

        implicit def evLong[L <: Long, LW](
          implicit
          dsn : DFDesign,
          lCheck : `N >= 0`.Long.CheckedShell[L],
          w : BitsWidthOf.LongAux[L, LW]
        ) : Aux[L, DFUInt[LW]] = createConst[L, LW](left => {
          lCheck.unsafeCheck(left)
          DFUInt.Token(w(left), left)
        })

        implicit def evBigInt[L <: BigInt](
          implicit
          dsn : DFDesign
        ) : Aux[L, DFUInt[Int]] = createConst[L, Int](left => {
          `N >= 0`.BigInt.unsafeCheck(left)
          DFUInt.Token(left.bitsWidth, left)
        })
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}