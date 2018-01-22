package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.tokens._

trait DFUInt[W] extends DFAny.Val[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
  left =>
  import DFUInt.Operations._
  type Extendable
  def +[R](right: `Op+`.Able[R])(implicit op: `Op+`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def -[R](right: `Op-`.Able[R])(implicit op: `Op-`.Builder[DFUInt[W], Extendable, R]) = op(left, right)

  def extBy[N](numOfBits : Natural.Int.Checked[N])(
    implicit tfs : TwoFace.Int.Shell2[+, W, Int, N, Int]
  ) : DFUInt.Var[tfs.Out] = DFUInt.newVar(tfs(width, numOfBits)).init(getInit).assign(left)
//  def *  (right : DFUInt)         : DFUInt = ???
//  def /  (right : DFUInt)         : DFUInt = ???

  def <  [R](right: `Op<`.Able[R])(implicit op: `Op<`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def >  [R](right: `Op>`.Able[R])(implicit op: `Op>`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def <= [R](right: `Op<=`.Able[R])(implicit op: `Op<=`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def >= [R](right: `Op>=`.Able[R])(implicit op: `Op>=`.Builder[DFUInt[W], Extendable, R]) = op(left, right)

  def == [RW](right : DFUInt[RW])(implicit op: `Op==`.Builder[DFUInt[W], Extendable, DFUInt[RW]]) = op(left, right)
  def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def == [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def == (that : BigInt)(implicit op: `Op==`.Builder[DFUInt[W], Extendable, BigInt]) = op(left, that)
  def != [RW](right : DFUInt[RW])(implicit op: `Op!=`.Builder[DFUInt[W], Extendable, DFUInt[RW]]) = op(left, right)
  def != [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def != [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[DFUInt[W], Extendable, R]) = op(left, right)
  def != (that : BigInt)(implicit op: `Op!=`.Builder[DFUInt[W], Extendable, BigInt]) = op(left, that)

  def isZero = left == 0
  def isNonZero = left != 0
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

  implicit class FromInt(value : Int) {
    import DFUInt.Operations._
//    type Extendable
//    def extendable[Out <: XInt](implicit g : OpAuxInt[AcceptNonLiteral[GetLHSArg0], Out]) : FromInt[g.Out] with DFUInt.Extendable = new FromInt[g.Out](g.value) with DFUInt.Extendable
    def +  [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op+`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def -  [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op-`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def <  [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op<`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def >  [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op>`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def <= [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op<=`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
    def >= [L, RW](right : DFUInt[RW])(implicit left : GetLHSArg.Aux[ZeroI, L], op: `Op>=`.Builder[L, Extendable, DFUInt[RW]]) = op(left, right)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Operations {
    type Enabled = DummyImplicit
    type Disabled = Nothing

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Implicit configuration of when operation is possible
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait General[IntEn, LongEn, BigIntEn, DFUIntEn] {
      abstract class Able[R](val right : R)

      trait BuilderTop[L, LE, R] {
        type Comp
        def apply(left : L, rightR : R) : Comp
      }

      object Able {
        implicit def fromAble[R](able : Able[R]) : R = able.right
//        implicit def ofInt(value : Int)(implicit e : IntEn, g : AcceptNonLiteral[GetArg0]) : Able[g.Out] =
//          new Able[g.Out](g.value) {}
//        implicit def ofLong(value : Long)(implicit e : LongEn, g : AcceptNonLiteral[GetArg0]) : Able[g.Out] =
//          new Able[g.Out](g.value) {}
        implicit def ofInt(value : Int)(implicit e : IntEn) : Able[Int] =
          new Able[Int](value) {}
        implicit def ofXInt[R <: XInt](value : R)(implicit e : IntEn) : Able[R] =
          new Able[R](value) {}
        implicit def ofLong(value : Long)(implicit e : LongEn) : Able[Long] =
          new Able[Long](value) {}
        implicit def ofXLong[R <: XLong](value : R)(implicit e : LongEn) : Able[R] =
          new Able[R](value) {}
        implicit def ofBigInt(value : BigInt)(implicit e : BigIntEn) : Able[BigInt] =
          new Able[BigInt](value) {}
        implicit class OfDFUInt[RW0](value : DFUInt[RW0])(implicit e : DFUIntEn) extends Able[DFUInt[RW0]](value)
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // +/- operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) extends General[Enabled, Enabled, Enabled, Enabled] {
      //NCW = No-carry width
      //WCW = With-carry width
      case class Component[NCW, WCW](wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        val c = wc.bits().msbit
      }

      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
      trait Builder[L, LE, R] extends BuilderTop[L, LE, R]

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
        object `L >= 0` {
          type MsgCommon[L] = "Unsigned substraction of a DF variable from a negative literal is not permitted. Found literal: " + ToString[L]
          type `OpIs+` = ImplicitFound[K =:= `Ops+Or-`.+]
          object Int extends Checked0Param.Int {
            type Cond[L] = `OpIs+` || (L >= 0)
            type Msg[L] = MsgCommon[L]
          }
          object Long extends Checked0Param.Long {
            type Cond[L] = `OpIs+` || (L >= 0L)
            type Msg[L] = MsgCommon[L]
          }
          object BigInt extends Checked1Param.Boolean {
            type Cond[T, P] = T
            type Msg[T, P] = MsgCommon[P]
            type ParamFace = String
            def unsafeCheck(r : BigInt)(implicit chk : BigInt.CheckedShell[Boolean, String]) : Unit =
              chk.unsafeCheck(r >= 0, r.toString())
          }
        }

        object Inference {
          import singleton.ops.math.Max
          type CalcWC[LW, RW] = Max[LW, RW] + 1
          type WC[LW, RW] = TwoFace.Int.Shell2[CalcWC, LW, Int, RW, Int]
          type CalcNC[LW, RW] = Max[LW, RW]
          type NC[LW, RW] = TwoFace.Int.Shell2[CalcNC, LW, Int, RW, Int]
        }

        trait DetailedBuilder[L, LW, LE, R, RW] {
          type Comp
          def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
        }
        object DetailedBuilder {
          implicit def ev[L, LW, LE, R, RW](
            implicit
            ncW : Inference.NC[LW, RW],
            wcW : Inference.WC[LW, RW],
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
            }
        }

        def create[L, LW, LE, R, RW](properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW]))(
          implicit
          ncW : Inference.NC[LW, RW],
          wcW : Inference.WC[LW, RW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : Aux[L, LE, R, Component[ncW.Out, wcW.Out]] = new Builder[L, LE, R] {
          type Comp = Component[ncW.Out, wcW.Out]
          def apply(leftL : L, rightR : R) : Comp = {
            val (creationKind, left, right) = properLR(leftL, rightR)
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
        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
          implicit
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((left, right) => (kind, left, right))

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, LE, R <: Int, RW](
          implicit
          rW : BitsWidthOf.IntAux[Abs[R], RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[RW](TokenUInt(rW(right), right)))
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, LE, R <: Long, RW](
          implicit
          rW : BitsWidthOf.LongAux[Abs[R], RW],
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[RW](TokenUInt(rW(right), right)))
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW, LE](
          implicit
          detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, BigInt, Int]
        ) = detailedBuilder((left, rightNum) => {
          val (creationKind, right) = if (rightNum >= 0) (kind, rightNum) else (-kind, -rightNum)
          (creationKind, left, DFUInt.const[Int](TokenUInt(right.bitsWidth, right)))
        })

        implicit def evInt_op_DFUInt[L <: Int, LW, LE, R <: DFUInt[RW], RW](
          implicit
          lCheck : `L >= 0`.Int.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.IntAux[Abs[L], LW],
          detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
        ) = detailedBuilder((leftNum, right) => {
          lCheck.unsafeCheck(leftNum)
          (kind, DFUInt.const[LW](TokenUInt(lW(leftNum), leftNum)), right)
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
      type + = +.type
      case object - extends Kind {
        def unary_- : Kind = `Ops+Or-`.+
      }
      type - = -.type
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
      @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
      trait Builder[L, LE, R] extends BuilderTop[L, LE, R]

      object Builder {
        type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
          type Comp = Comp0
        }

        object `LW == RW` extends Checked1Param.Int {
          type Cond[LW, RW] = LW == RW
          type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
          type ParamFace = Int
        }

        object `VecW >= ConstW` extends Checked1Param.Int { //Needs to be mitigated to a warning
          type Cond[VW, CW] = VW >= CW
          type Msg[VW, CW] = "A static boolean result detected, due to an unsigned comparison between a DF variable and a larger literal. Found: LHS-width = "+ ToString[VW] + " and RHS-width = " + ToString[CW]
          type ParamFace = Int
        }

        object `N >= 0` {
          type MsgCommon[R] = "Unsigned comparison operations do not support negative numbers. Found: " + ToString[R]
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

        def create[L, LW, LE, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Aux[L, LE, R, DFBool] =
          new Builder[L, LE, R] {
          type Comp = DFBool
          def apply(leftL : L, rightR : R) : Comp = {
            val (left, right) = properLR(leftL, rightR)
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

        implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
          implicit
          checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_,_], LW, RW]
        ) : Aux[DFUInt[LW], LE, DFUInt[RW], DFBool] =
          create[DFUInt[LW], LW, LE, DFUInt[RW], RW]((left, right) => {
            checkLWvRW.unsafeCheck(left.width, right.width)
            (left, right)
          })

        implicit def evDFUInt_op_Int[L <: DFUInt[LW], LW, LE, R <: Int, RW](
          implicit
          checkR : `N >= 0`.Int.CheckedShellSym[Builder[_,_,_], R],
          rW : BitsWidthOf.IntAux[R, RW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
        ) : Aux[DFUInt[LW], LE, R, DFBool] = create[DFUInt[LW], LW, LE, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](TokenUInt(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_Long[L <: DFUInt[LW], LW, LE, R <: Long, RW](
          implicit
          checkR : `N >= 0`.Long.CheckedShellSym[Builder[_,_,_], R],
          rW : BitsWidthOf.LongAux[R, RW],
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
        ) : Aux[DFUInt[LW], LE, R, DFBool] = create[DFUInt[LW], LW, LE, R, RW]((left, rightNum) => {
          checkR.unsafeCheck(rightNum)
          val right = DFUInt.const[RW](TokenUInt(rW(rightNum), rightNum))
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

        implicit def evDFUInt_op_BigInt[L <: DFUInt[LW], LW, LE](
          implicit
          checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, Int]
        ) : Aux[DFUInt[LW], LE, BigInt, DFBool] = create[DFUInt[LW], LW, LE, BigInt, Int]((left, rightNum) => {
          `N >= 0`.BigInt.unsafeCheck(rightNum)
          val right = DFUInt.const[Int](TokenUInt(rightNum.bitsWidth, rightNum))
          (left, right)
        })

        implicit def evInt_op_DFUInt[L <: Int, LW, LE, R <: DFUInt[RW], RW](
          implicit
          checkL : `N >= 0`.Int.CheckedShellSym[Builder[_,_,_], L],
          lW : BitsWidthOf.IntAux[L, LW],
//          checkLWvRW : `LW != RW`.CheckedShellSym[Builder[_,_,_], LW, RW]
        ) : Aux[L, LE, DFUInt[RW], DFBool] = create[L, LW, LE, DFUInt[RW], RW]((leftNum, right) => {
          checkL.unsafeCheck(leftNum)
          (DFUInt.const[LW](TokenUInt(lW(leftNum), leftNum)), right)
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