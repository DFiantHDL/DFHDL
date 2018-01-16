package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.tokens._

sealed trait DFUInt[W] extends DFAny.Val[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
  import DFUInt.Operations._
  type Extendable
  def +[R](that: `Op+`.Able[R])(implicit op: `Op+`.Builder[W, Extendable, R]) = op(this, that)
  def -[R](that: `Op-`.Able[R])(implicit op: `Op-`.Builder[W, Extendable, R]) = op(this, that)

  def extBy[N](numOfBits : Natural.Int.Checked[N])(
    implicit tfs : TwoFace.Int.Shell2[+, W, Int, N, Int]
  ) : DFUInt.Var[tfs.Out] = DFUInt.newVar(tfs(width, numOfBits)).init(getInit).assign(this)
//  def *  (that : DFUInt)         : DFUInt = ???
//  def /  (that : DFUInt)         : DFUInt = ???

  def <  [R](that: `Op<`.Able[R])(implicit op: `Op<`.Builder[W, Extendable, R]) = op(this, that)
  def >  [R](that: `Op>`.Able[R])(implicit op: `Op>`.Builder[W, Extendable, R]) = op(this, that)
  def <= [R](that: `Op<=`.Able[R])(implicit op: `Op<=`.Builder[W, Extendable, R]) = op(this, that)
  def >= [R](that: `Op>=`.Able[R])(implicit op: `Op>=`.Builder[W, Extendable, R]) = op(this, that)

  def == [RW](that : DFUInt[RW])(implicit op: `Op==`.Builder[W, Extendable, DFUInt[RW]]) = op(this, that)
  def == [R](that : Int)(implicit g : OpAuxGen[AcceptNonLiteral[GetArg0], R], op: `Op==`.Builder[W, Extendable, R]) = op(this, g.value)
  def == [R](that : Long)(implicit g : OpAuxGen[AcceptNonLiteral[GetArg0], R], op: `Op==`.Builder[W, Extendable, R]) = op(this, g.value)
  def == (that : BigInt)(implicit op: `Op==`.Builder[W, Extendable, BigInt]) = op(this, that)
  def != [RW](that : DFUInt[RW])(implicit op: `Op!=`.Builder[W, Extendable, DFUInt[RW]]) = op(this, that)
  def != [R](that : Int)(implicit g : OpAuxGen[AcceptNonLiteral[GetArg0], R], op: `Op!=`.Builder[W, Extendable, R]) = op(this, g.value)
  def != [R](that : Long)(implicit g : OpAuxGen[AcceptNonLiteral[GetArg0], R], op: `Op!=`.Builder[W, Extendable, R]) = op(this, g.value)
  def != (that : BigInt)(implicit op: `Op!=`.Builder[W, Extendable, BigInt]) = op(this, that)

  def isZero = this == 0
  def isNonZero = this != 0
//  def toDFSInt[SW](implicit tfs : TwoFace.Int.)
  def dfTypeName : String = "DFUInt"
  def extendable : DFUInt[W] with DFUInt.Extendable = DFUInt.extendable[W](this)
}


object DFUInt {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Var[W] extends DFUInt[W] with DFAny.Var[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
//    final def := (that : BigInt) : this.type = ???
//    final def := (that : Int) : this.type = ???
//    final def := (that : Long) : this.type = ???
  }

  trait Extendable {
    type Extendable = true
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit) : Var[W] = newVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = newVar(checkedWidth.unsafeCheck())
  //  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : BigInt) : Var = apply(bigIntRepWidth(supLimit-1))
  //  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar[W](width : TwoFace.Int[W]) : Var[W] =
    new DFAny.NewVar(width, Seq(TokenUInt(width, 0))) with Var[W]

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[TokenUInt] = Seq()) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W]

  protected[DFiant] def extendable[W](extendedVar : DFUInt[W]) : Var[W] with Extendable =
    new DFAny.Alias(extendedVar, extendedVar.width, 0) with Var[W] with Extendable

  protected[DFiant] def const[W](token : TokenUInt) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[TokenUInt], args : DFAny*) : DFUInt[W] =
    new DFAny.Op(width, opString, opInit, args) with DFUInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Operations {
    object `LW >= RW` extends Checked1Param.Int {
      type Cond[LW, RW] = LW >= RW
      type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
      type ParamFace = Int
      type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
    }
    object `R >= 0` {
      type MsgCommon[R] = "Number must be natural. Received: " + ToString[R]
      object Int extends Checked0Param.Int {
        type Cond[R] = R >= 0
        type Msg[R] = MsgCommon[R]
      }
      object Long extends Checked0Param.Long {
        type Cond[R] = R >= 0L
        type Msg[R] = MsgCommon[R]
      }
      object BigInt extends Checked1Param.Boolean {
        type Cond[T, P] = T
        type Msg[T, P] = MsgCommon[P]
        type ParamFace = String
        def unsafeCheck(r : BigInt)(implicit chk : BigInt.CheckedShell[Boolean, String]) : Unit =
          chk.unsafeCheck(r >= 0, r.toString())
      }
    }
    type Enabled = DummyImplicit
    type Disabled = Nothing

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Implicit configuration of when operation is possible
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait General[IntEn, LongEn, BigIntEn, DFUIntEn] {
      abstract class Able[R](val right : R)

      trait BuilderTop[LW, LE, R] {
        type Comp
        def apply(left : DFUInt[LW], rightR : R) : Comp
      }

      object Able {
        implicit def fromAble[R](able : Able[R]) : R = able.right
        implicit def ofInt(value : Int)(implicit e : IntEn, g : AcceptNonLiteral[GetArg0]) : Able[g.Out] =
          new Able[g.Out](g.value) {}
        implicit def ofLong(value : Long)(implicit e : LongEn, g : AcceptNonLiteral[GetArg0]) : Able[g.Out] =
          new Able[g.Out](g.value) {}
        implicit def ofBigInt(value : BigInt)(implicit e : BigIntEn) : Able[BigInt] =
          new Able[BigInt](value) {}
        implicit class OfDFUInt[RW0](value : DFUInt[RW0])(implicit e : DFUIntEn) extends Able[DFUInt[RW0]](value)
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // +/- operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected abstract class `Ops+Or-`(kind : `Ops+Or-`.Kind) extends General[Enabled, Enabled, Enabled, Enabled] {
      //NCW = No-carry width
      //WCW = With-carry width
      case class Component[NCW, WCW](wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        val c = wc.bits().msbit
      }

      @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support Ops `+` or `-` with the type ${R}")
      trait Builder[LW, LE, R] extends BuilderTop[LW, LE, R]

      object Builder {
        type Aux[LW, LE, R, Comp0] = Builder[LW, LE, R] {
          type Comp = Comp0
        }
        object Inference {
          import singleton.ops.math.Max
          type CalcWC[LW, RW] = Max[LW, RW] + 1
          type WC[LW, RW] = TwoFace.Int.Shell2[CalcWC, LW, Int, RW, Int]
          type CalcNC[LW, RW] = Max[LW, RW]
          type NC[LW, RW] = TwoFace.Int.Shell2[CalcNC, LW, Int, RW, Int]
        }

        def create[LW, LE, R, RW](properR : R => (`Ops+Or-`.Kind, DFUInt[RW]))(
          implicit
          ncW : Inference.NC[LW, RW],
          wcW : Inference.WC[LW, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[LW, LE, R, Component[ncW.Out, wcW.Out]] = new Builder[LW, LE, R] {
          type Comp = Component[ncW.Out, wcW.Out]
          def apply(left : DFUInt[LW], rightR : R) : Comp = {
            val (creationKind, right) = properR(rightR)
            // Completing runtime checks
            checkLWvRW.unsafeCheck(left.width, right.width)
            // Constructing op
            val wc = creationKind match {
              case `Ops+Or-`.+ =>
                DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
              case `Ops+Or-`.- =>
                DFUInt.op[wcW.Out](wcW(left.width, right.width), "-", left.getInit - right.getInit, left, right)
            }
            // Creating extended component aliasing the op
            Component[ncW.Out, wcW.Out](wc)
          }
        }

        import singleton.ops.math.Abs
        implicit def evDFUInt[LW, LE, R <: DFUInt[RW], RW](
          implicit
          ncW : Inference.NC[LW, RW],
          wcW : Inference.WC[LW, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) = create[LW, LE, DFUInt[RW], RW](right => (kind, right))

        implicit def evInt[LW, LE, R <: Int, RW](
          implicit
          rW : BitsWidthOf.IntAux[Abs[R], RW],
          ncW : Inference.NC[LW, RW],
          wcW : Inference.WC[LW, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) = create[LW, LE, R, RW](rightNum => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, DFUInt.const[RW](TokenUInt(rW(right), right)))
        })

        implicit def evLong[LW, LE, R <: Long, RW](
          implicit
          rW : BitsWidthOf.LongAux[Abs[R], RW],
          ncW : Inference.NC[LW, RW],
          wcW : Inference.WC[LW, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) = create[LW, LE, R, RW](rightNum => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, DFUInt.const[RW](TokenUInt(rW(right), right)))
        })

        implicit def evBigInt[LW, LE](
          implicit
          ncW : Inference.NC[LW, Int],
          wcW : Inference.WC[LW, Int],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, Int]
        ) = create[LW, LE, BigInt, Int](rightNum => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, DFUInt.const[Int](TokenUInt(right.bitsWidth, right)))
        })
      }
    }
    protected object `Ops+Or-` {
      sealed trait Kind {
        def unary_- : Kind
      }
      case object + extends Kind {
        def unary_- : Kind = `Ops+Or-`.-
      }
      case object - extends Kind {
        def unary_- : Kind = `Ops+Or-`.+
      }
    }
    object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
    object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // * operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Comparison operations
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected abstract class OpsCompare(kind : OpsCompare.Kind) extends General[Enabled, Enabled, Enabled, Enabled] {
      @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support Comparison Ops with the type ${R}")
      trait Builder[LW, LE, R] extends BuilderTop[LW, LE, R]

      object Builder {
        type Aux[LW, LE, R, Comp0] = Builder[LW, LE, R] {
          type Comp = Comp0
        }

        def create[LW, LE, R, RW](properR : R => DFUInt[RW])(
          implicit
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[LW, LE, R, DFBool] = new Builder[LW, LE, R] {
          type Comp = DFBool
          def apply(left : DFUInt[LW], rightAble : R) : Comp = {
            val right = properR(rightAble)
            // Completing runtime checks
            checkLWvRW.unsafeCheck(left.width, right.width)
            // Constructing op
            kind match {
              case OpsCompare.== => DFBool.op("==", TokenUIntSeq(left.getInit) == right.getInit, left, right)
              case OpsCompare.!= => DFBool.op("!=", TokenUIntSeq(left.getInit) != right.getInit, left, right)
              case OpsCompare.<  => DFBool.op("<",  TokenUIntSeq(left.getInit) <  right.getInit, left, right)
              case OpsCompare.>  => DFBool.op(">",  TokenUIntSeq(left.getInit) >  right.getInit, left, right)
              case OpsCompare.<= => DFBool.op("<=", TokenUIntSeq(left.getInit) <= right.getInit, left, right)
              case OpsCompare.>= => DFBool.op(">=", TokenUIntSeq(left.getInit) >= right.getInit, left, right)
            }
          }
        }

        implicit def evDFUInt[LW, LE, R <: DFUInt[RW], RW](
          implicit
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[LW, LE, DFUInt[RW], DFBool] = create[LW, LE, DFUInt[RW], RW](right => right)

        implicit def evInt[LW, LE, R <: Int, RW](
          implicit
          checkR : `R >= 0`.Int.CheckedShellSym[Builder[_,_,_], R],
          rW : BitsWidthOf.IntAux[R, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[LW, LE, R, DFBool] = create[LW, LE, R, RW](rightNum => {
          checkR.unsafeCheck(rightNum)
          DFUInt.const[RW](TokenUInt(rW(rightNum), rightNum))
        })

        implicit def evLong[LW, LE, R <: Long, RW](
          implicit
          checkR : `R >= 0`.Long.CheckedShellSym[Builder[_,_,_], R],
          rW : BitsWidthOf.LongAux[R, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[LW, LE, R, DFBool] = create[LW, LE, R, RW](rightNum => {
          checkR.unsafeCheck(rightNum)
          DFUInt.const[RW](TokenUInt(rW(rightNum), rightNum))
        })

        implicit def evBigInt[LW, LE](
          implicit
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, Int]
        ) : Aux[LW, LE, BigInt, DFBool] = create[LW, LE, BigInt, Int](rightNum => {
          `R >= 0`.BigInt.unsafeCheck(rightNum)
          DFUInt.const[Int](TokenUInt(rightNum.bitsWidth, rightNum))
        })
      }
    }
    protected object OpsCompare {
      sealed trait Kind
      case object == extends Kind
      case object != extends Kind
      case object <  extends Kind
      case object >  extends Kind
      case object <= extends Kind
      case object >= extends Kind
    }
    object `Op==` extends OpsCompare(OpsCompare.==)
    object `Op!=` extends OpsCompare(OpsCompare.!=)
    object `Op<`  extends OpsCompare(OpsCompare.<)
    object `Op>`  extends OpsCompare(OpsCompare.>)
    object `Op<=` extends OpsCompare(OpsCompare.<=)
    object `Op>=` extends OpsCompare(OpsCompare.>=)
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}