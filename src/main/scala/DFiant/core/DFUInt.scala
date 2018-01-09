package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.tokens._

trait DFUInt[W] extends DFAny.Val[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
  import DFUInt.Operations._
  def +[R, RW](that: `Op+`.Able.Aux[R, RW])(implicit op: `Op+`.Builder[W, R, RW]) = op(this, that)
//  def -[R](that: `OpEx`.Able[DFUInt[W], R])(implicit errChk: that.ErrChk) = that(this)
//  def extBy(numOfBits : Int)     : TAlias = ???
//  def +  (that : DFUInt)         : DFUInt = ???
//  def -  (that : DFUInt)         : DFUInt = ???
//  def *  (that : DFUInt)         : DFUInt = ???
//  def /  (that : DFUInt)         : DFUInt = ???
//  def == (that : Int)            : DFBool = ???
//  def == (that : Long)           : DFBool = ???
//  def == (that : BigInt)         : DFBool = ???
//  def != (that : Int)            : DFBool = ???
//  def != (that : Long)           : DFBool = ???
//  def != (that : BigInt)         : DFBool = ???
//  def isZero                     : DFBool = ???
//  def isNonZero                  : DFBool = ???
//  def <  (that : DFUInt)         : DFBool = ???
//  def >= (that : DFUInt)         : DFBool = ???
//  def >  (that : DFUInt)         : DFBool = ???
//  def <= (that : DFUInt)         : DFBool = ???
  def dfTypeName : String = "DFUInt"
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

  protected[DFiant] def const[W](token : TokenUInt) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[TokenUInt], args : DFAny*) : DFUInt[W] =
    new DFAny.Op(width, opString, opInit, args) with DFUInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected object Operations {
    object `LW >= RW` extends Checked1Param.Int {
      type Cond[LW, RW] = LW >= RW
      type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
      type ParamFace = Int
    }
    object `R >= 0` {
      object Int extends Checked0Param.Int {
        type Cond[R] = ITE[IsInt[R], R > 0, true]
        type Msg[R] = "Number must be natural. Received: " + ToString[R]
      }
      object Long extends Checked0Param.Long {
        type Cond[R] = ITE[IsLong[R], R > 0, true]
        type Msg[R] = "Number must be natural. Received: " + ToString[R]
      }
    }
    type Enabled = DummyImplicit
    type Disabled = Nothing

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Implicit configuration of when operation is possible
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait General[IntEn, LongEn, DFUIntEn] {
      abstract class Able[R](val right : R) {
        type RW
        val width : TwoFace.Int[RW]
        def dfVar : DFUInt[RW]
      }

      trait BuilderTop[LW, R, RW] {
        type Comp
        def apply(left : DFUInt[LW], rightAble : Able.Aux[R, RW]) : Comp
      }

      object Able {
        type Aux[R, RW0] = Able[R]{type RW = RW0}
        implicit def ofInt(right : Int)(implicit e : IntEn, w : BitsWidthOf.Int[Int]) : Aux[Int, w.Out] = new Able[Int](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofXInt[R <: XInt](right : R)(implicit e : IntEn, w : BitsWidthOf.Int[R]) : Aux[R, w.Out] = new Able[R](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofLong(right : Long)(implicit e : LongEn, w : BitsWidthOf.Long[Long]) : Aux[Long, w.Out] = new Able[Long](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofXLong[R <: XLong](right : R)(implicit e : LongEn, w : BitsWidthOf.Long[R]) : Aux[R, w.Out] = new Able[R](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit class OfDFUInt[RW0](right : DFUInt[RW0])(implicit e : DFUIntEn) extends Able[DFUInt[RW0]](right) {
          type RW = RW0
          val width : TwoFace.Int[RW] = right.width
          def dfVar : DFUInt[RW] = right
        }
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Addition operation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object `Op+` extends General[Enabled, Enabled, Enabled] {
      //NCW = No-carry width
      //WCW = With-carry width
      abstract class Component[NCW, WCW](val wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        val c = wc.bits().msbit
      }

      @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support addition of type ${R}")
      trait Builder[LW, R, RW] extends BuilderTop[LW, R, RW]

      object Builder {
        type Aux[LW, R, RW, Comp0] = Builder[LW, R, RW] {
          type Comp = Comp0
        }
        object AdderWidth {
          import singleton.ops.math.Max
          type CalcWC[LW, RW] = Max[LW, RW] + 1
          type WC[LW, RW] = TwoFace.Int.Shell2[CalcWC, LW, Int, RW, Int]
          type CalcNC[LW, RW] = Max[LW, RW]
          type NC[LW, RW] = TwoFace.Int.Shell2[CalcNC, LW, Int, RW, Int]
        }

        implicit def ev[LW, R, RW](
          implicit
          ncW : AdderWidth.NC[LW, RW],
          wcW : AdderWidth.WC[LW, RW],
          checkRInt  : `R >= 0`.Int.CheckedShellSym[Builder[_,_,_], R],
          checkRLong : `R >= 0`.Long.CheckedShellSym[Builder[_,_,_], R],
          checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_,_], LW, RW]
        ) : Aux[LW, R, RW, Component[ncW.Out, wcW.Out]] =
          new Builder[LW, R, RW] {
            type Comp = Component[ncW.Out, wcW.Out]
            def apply(left : DFUInt[LW], rightAble : Able.Aux[R, RW]) : Comp = {
              ////////////////////////////////////////////////////////////
              // Completing runtime checks
              ////////////////////////////////////////////////////////////
              rightAble.right match {
                case t : Int => checkRInt.unsafeCheck(t)
                case t : Long => checkRLong.unsafeCheck(t)
                case _ => //No other check required
              }
              checkLWvRW.unsafeCheck(left.width, rightAble.width)
              ////////////////////////////////////////////////////////////
              val right = rightAble.dfVar
              val wc = DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
              new Component[ncW.Out, wcW.Out](wc) {
              }
            }
          }
      }
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  }
}