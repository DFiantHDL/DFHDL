package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.tokens._

trait DFUInt[W] extends DFAny.Val[W, TokenUInt, DFUInt[W], DFUInt.Var[W]] {
  import DFUInt.Operations._
  def +[R](that: `Op+`.Able[R])(implicit op: `Op+`.Builder[W, R]) = op(this, that)
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


  object Operations {
    object `Op+` {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Implicit configuration of when operation is possible
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////
      abstract class Able[R](val right : R)

      object Able {
        implicit class OfInt(right : Int) extends Able[Int](right)
        implicit class OfXInt[R <: XInt](right : R) extends Able[R](right)
        implicit class OfDFUInt[RW](right : DFUInt[RW]) extends Able[DFUInt[RW]](right)
      }
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////

      abstract class AdderUInt[NCW, WCW](val wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
        val c = wc.bits().msbit
      }

      @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support addition of type ${R}")
      trait Builder[LW, R] {
        type NCW //No-carry width
        type WCW //With-carry width
        def apply(left : DFUInt[LW], rightAble : Able[R]) : AdderUInt[NCW, WCW]
      }

      object Builder {
        type Aux[LW, R, Ret_NCW, Ret_WCW] = Builder[LW, R] {
          type NCW = Ret_NCW
          type WCW = Ret_WCW
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
          type Check[LW, RW] = Checked.Shell2Sym[>=, Msg, Builder[_,_], LW, Int, RW, Int]
        }
        object `R >= 0` {
          type Cond[R] = R >= 0
          type Msg[R] = "Number must be natural. Received: " + ToString[R]
          type Check[R] = Checked.Shell1Sym[Cond, Msg, Builder[_,_], R, Int]
        }

        def createUInt[LW, R, RW](ra2r : Able[R] => DFUInt[RW])(
          implicit
          ncW : AdderWidth.NC[LW, RW],
          wcW : AdderWidth.WC[LW, RW],
          check : `LW >= RW`.Check[LW, RW]
        ) : Aux[LW, R, ncW.Out, wcW.Out] =
          new Builder[LW, R] {
            type NCW = ncW.Out
            type WCW = wcW.Out
            def apply(left : DFUInt[LW], rightAble : Able[R]) : AdderUInt[ncW.Out, wcW.Out] = {
              val right = ra2r(rightAble)
              check.unsafeCheck(left.width, right.width)
              val wc = DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
              new AdderUInt[ncW.Out, wcW.Out](wc) {
              }
            }
          }

        implicit def evUInt[LW, RW](
          implicit
          ncW : AdderWidth.NC[LW, RW],
          wcW : AdderWidth.WC[LW, RW],
          check : `LW >= RW`.Check[LW, RW]
        ) = createUInt[LW, DFUInt[RW], RW](t => t.right)

        implicit def evNum[LW, R <: Int, RW](
          implicit
          bitsWidthOf : BitsWidthOf.IntAux[R, RW],
          ncW : AdderWidth.NC[LW, RW],
          wcW : AdderWidth.WC[LW, RW],
          check : `LW >= RW`.Check[LW, RW]
        ) = createUInt[LW, R, RW](t => DFUInt.const[RW](TokenUInt(bitsWidthOf(t.right), t.right)))
      }

    }
  }
}