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
    implicit class DFBitsInt[LW](right : Int) extends
      BinaryOpRightConstInt[DFBits[LW], Int](right) with Able[DFBits[LW], Int]
    implicit class DFBitsXInt[LW, R <: XInt](right : R) extends
      BinaryOpRightConstInt[DFBits[LW], R](right) with Able[DFBits[LW], R]
    implicit class DFBitsDFBits[LW, RW](right : DFBits[RW]) extends
      BinaryOpRightDFVar[DFBits[LW], DFBits[RW]](right) with Able[DFBits[LW], DFBits[RW]]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class AdderBits[NCW, WCW](val wc : DFBits[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFBits[NCW] {
    val c = wc.msbit
  }

  @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support addition of type ${R}")
  trait Builder[L <: DFAny, R] {
    type Out[W] <: DFAnyW[W]
    type NCW //No carry width
    type WCW //With carry width
    def apply(left : L, rightAble : Able[L, R]) : AdderBits[NCW, WCW]
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

    def createBits[LW, R, RW](
      implicit
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) : Aux[DFBits[LW], R, DFBits, ncW.Out, wcW.Out] =
      new Builder[DFBits[LW], R] {
        type Out[W] = DFBits[W]
        type NCW = ncW.Out
        type WCW = wcW.Out
        def apply(left : DFBits[LW], rightAble : Able[DFBits[LW], R]) : AdderBits[ncW.Out, wcW.Out] = {
          val right = rightAble.asDFAny
          check.unsafeCheck(left.width, right.width)
          val wc = DFBits.op[wcW.Out](wcW(left.width, right.width), "+", left.almanacEntry.init + right.almanacEntry.init, left, right)
          new AdderBits[ncW.Out, wcW.Out](wc) {
          }
        }
      }

    implicit def evBits[LW, L <: DFBits[LW], RW, R <: DFBits[RW]](
      implicit
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) = createBits[LW, DFBits[RW], RW]

    implicit def evNum[LW, L <: DFBits[LW], RW, R <: Int](
      implicit
      bitsWidthOf : BitsWidthOf.IntAux[R, RW],
      ncW : AdderWidth.NC[LW, RW],
      wcW : AdderWidth.WC[LW, RW],
      check : `LW >= RW`.Check[LW, RW]
    ) = createBits[LW, R, RW]
  }

}




