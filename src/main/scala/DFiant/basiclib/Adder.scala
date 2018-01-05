package DFiant.basiclib

import DFiant.core._
import DFiant.internals._
import DFiant.tokens._

import singleton.ops._
import singleton.ops.math.Max
import singleton.twoface._

object `Op+` {
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicit configuration of when operation is possible
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Able[L <: DFAny, R] extends BinaryOpRight[L, R]

  object Able {
    implicit class DFUIntInt[LW](right : Int) extends
      BinaryOpRightConstInt[DFUInt[LW], Int](right) with Able[DFUInt[LW], Int]
    implicit class DFUIntXInt[LW, R <: XInt](right : R) extends
      BinaryOpRightConstInt[DFUInt[LW], R](right) with Able[DFUInt[LW], R]
    implicit class DFUIntDFUInt[LW, RW](right : DFUInt[RW]) extends
      BinaryOpRightDFVar[DFUInt[LW], DFUInt[RW]](right) with Able[DFUInt[LW], DFUInt[RW]]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class AdderUInt[NCW, WCW](val wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
    val c = wc.bits().msbit
  }

  @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support addition of type ${R}")
  trait Builder[L <: DFAny, R] {
    type Out[W] <: DFAnyW[W]
    type NCW //No-carry width
    type WCW //With-carry width
    def apply(left : L, rightAble : Able[L, R]) : AdderUInt[NCW, WCW]
  }

  object Builder {
    type Aux[L <: DFAny, R, Ret_Out[W], Ret_NCW, Ret_WCW] = Builder[L, R] {
      type Out[W] = Ret_Out[W]
      type NCW = Ret_NCW
      type WCW = Ret_WCW
    }
    object AdderWidth {
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

    def createUInt[LW, R, RW](
      implicit
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) : Aux[DFUInt[LW], R, DFUInt, ncW.Out, wcW.Out] =
      new Builder[DFUInt[LW], R] {
        type Out[W] = DFUInt[W]
        type NCW = ncW.Out
        type WCW = wcW.Out
        def apply(left : DFUInt[LW], rightAble : Able[DFUInt[LW], R]) : AdderUInt[ncW.Out, wcW.Out] = {
          val right = rightAble.asDFAny.asInstanceOf[Out[RW]]
          check.unsafeCheck(left.width, right.width)
          val wc = DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
          new AdderUInt[ncW.Out, wcW.Out](wc) {
          }
        }
      }

    implicit def evUInt[LW, L <: DFUInt[LW], RW, R <: DFUInt[RW]](
      implicit
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) = createUInt[LW, DFUInt[RW], RW]

    implicit def evNum[LW, L <: DFUInt[LW], RW, R <: Int](
      implicit
      bitsWidthOf : BitsWidthOf.IntAux[R, RW],
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) = createUInt[LW, R, RW]
  }

}




