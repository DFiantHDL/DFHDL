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
  ///////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFUInt[W] with DFAny.Var[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
//    final def := (that : BigInt) : this.type = ???
//    final def := (that : Int) : this.type = ???
//    final def := (that : Long) : this.type = ???
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit) : Var[W] = newVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = newVar(checkedWidth.unsafeCheck())
  //  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : BigInt) : Var = apply(bigIntRepWidth(supLimit-1))
  //  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar[W](width : TwoFace.Int[W]) : Var[W] =
    new DFAny.NewVar(width, Seq(TokenUInt(width, 0))) with Var[W]

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[TokenUInt] = Seq()) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W]

  protected[DFiant] def const[W](token : TokenUInt) : DFUInt[W] =
    new DFAny.Const(token) with DFUInt[W]

  protected[DFiant] def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[TokenUInt], args : DFAny*) : DFUInt[W] =
    new DFAny.Op(width, opString, opInit, args) with DFUInt[W]
  ///////////////////////////////////////////////////////////////////////////////////////////


  protected object Operations {
    trait General {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Implicit configuration of when operation is possible
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////
      abstract class Able[R](val right : R) {
        type RW
        val width : TwoFace.Int[RW]
        def dfVar : DFUInt[RW]
      }

      object Able {
        type Aux[R, RW0] = Able[R]{type RW = RW0}
        implicit def ofInt(right : Int)(implicit w : BitsWidthOf.Int[Int]) : Aux[Int, w.Out] = new Able[Int](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofXInt[R <: XInt](right : R)(implicit w : BitsWidthOf.Int[R]) : Aux[R, w.Out] = new Able[R](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofLong(right : Long)(implicit w : BitsWidthOf.Long[Long]) : Aux[Long, w.Out] = new Able[Long](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit def ofXLong[R <: XLong](right : R)(implicit w : BitsWidthOf.Long[R]) : Aux[R, w.Out] = new Able[R](right) {
          type RW = w.Out
          val width : TwoFace.Int[RW] = w(right)
          def dfVar : DFUInt[RW] = DFUInt.const[RW](TokenUInt(width, right))
        }
        implicit class OfDFUInt[RW0](right : DFUInt[RW0]) extends Able[DFUInt[RW0]](right) {
          type RW = RW0
          val width : TwoFace.Int[RW] = right.width
          def dfVar : DFUInt[RW] = right
        }
      }
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    }
    object `Op+` extends General {

      //NCW = No-carry width
      //WCW = With-carry width
      abstract class Adder[NCW, WCW](val wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        val c = wc.bits().msbit
      }

      @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support addition of type ${R}")
      trait Builder[LW, R, RW] {
        type Component
        def apply(left : DFUInt[LW], rightAble : Able.Aux[R, RW]) : Component
      }

      object Builder {
        type Aux[LW, R, RW, Component0] = Builder[LW, R, RW] {
          type Component = Component0
        }
        object AdderWidth {
          import singleton.ops.math.Max
          type CalcWC[LW, RW] = Max[LW, RW] + 1
          type WC[LW, RW] = TwoFace.Int.Shell2[CalcWC, LW, Int, RW, Int]
          type CalcNC[LW, RW] = Max[LW, RW]
          type NC[LW, RW] = TwoFace.Int.Shell2[CalcNC, LW, Int, RW, Int]
        }
        object `LW >= RW` {
          type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
          type Check[LW, RW] = Checked.Shell2Sym[>=, Msg, Builder[_,_,_], LW, Int, RW, Int]
        }
        object `R >= 0` {
          type Cond[R] = R >= 0
          type Msg[R] = "Number must be natural. Received: " + ToString[R]
          type Check[R] = Checked.Shell1Sym[Cond, Msg, Builder[_,_,_], R, Int]
        }

        implicit def ev[LW, R, RW](
          implicit
          ncW : AdderWidth.NC[LW, RW],
          wcW : AdderWidth.WC[LW, RW],
          check : `LW >= RW`.Check[LW, RW]
        ) : Aux[LW, R, RW, Adder[ncW.Out, wcW.Out]] =
          new Builder[LW, R, RW] {
            type Component = Adder[ncW.Out, wcW.Out]
            def apply(left : DFUInt[LW], rightAble : Able.Aux[R, RW]) : Component = {
              val right = rightAble.dfVar
              check.unsafeCheck(left.width, right.width)
              val wc = DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
              new Adder[ncW.Out, wcW.Out](wc) {
              }
            }
          }
      }

    }
  }
}