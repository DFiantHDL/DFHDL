//package DFiant.basiclib
//
//import DFiant.core._
//import DFiant.internals._
//import DFiant.tokens._
//
//import singleton.ops._
//import singleton.ops.math.Max
//import singleton.twoface._
//
//object `Op+` {
//  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Implicit configuration of when operation is possible
//  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  abstract class Able[R](val right : R)
//
//  object Able {
//    implicit class OfInt(right : Int) extends Able[Int](right)
//    implicit class OfXInt[R <: XInt](right : R) extends Able[R](right)
//    implicit class OfDFUInt[RW](right : DFUInt[RW]) extends Able[DFUInt[RW]](right)
//  }
//  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  abstract class AdderUInt[NCW, WCW](val wc : DFUInt[WCW]) extends DFAny.Alias(wc, wc.width-1, 0) with DFUInt[NCW] {
//    val c = wc.bits().msbit
//  }
//
//  @scala.annotation.implicitNotFound("Dataflow variable DFUInt[${LW}] does not support addition of type ${R}")
//  trait Builder[LW, R] {
//    type NCW //No-carry width
//    type WCW //With-carry width
//    def apply(left : DFUInt[LW], rightAble : Able[R]) : AdderUInt[NCW, WCW]
//  }
//
//  object Builder {
//    type Aux[LW, R, Ret_NCW, Ret_WCW] = Builder[LW, R] {
//      type NCW = Ret_NCW
//      type WCW = Ret_WCW
//    }
//    object AdderWidth {
//      type CalcWC[LW, RW] = Max[LW, RW] + 1
//      type WC[LW, RW] = TwoFace.Int.Shell2[CalcWC, LW, Int, RW, Int]
//      type CalcNC[LW, RW] = Max[LW, RW]
//      type NC[LW, RW] = TwoFace.Int.Shell2[CalcNC, LW, Int, RW, Int]
//    }
//    object `LW >= RW` {
//      type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
//      type Check[LW, RW] = Checked.Shell2Sym[>=, Msg, Builder[_,_], LW, Int, RW, Int]
//    }
//    object `R >= 0` {
//      type Cond[R] = R >= 0
//      type Msg[R] = "Number must be natural. Received: " + ToString[R]
//      type Check[R] = Checked.Shell1Sym[Cond, Msg, Builder[_,_], R, Int]
//    }
//
//    def createUInt[LW, R, RW](ra2r : Able[R] => DFUInt[RW])(
//      implicit
//      ncW : AdderWidth.NC[LW, RW],
//      wcW : AdderWidth.WC[LW, RW],
//      check : `LW >= RW`.Check[LW, RW]
//    ) : Aux[LW, R, ncW.Out, wcW.Out] =
//      new Builder[LW, R] {
//        type NCW = ncW.Out
//        type WCW = wcW.Out
//        def apply(left : DFUInt[LW], rightAble : Able[R]) : AdderUInt[ncW.Out, wcW.Out] = {
//          val right = ra2r(rightAble)
//          check.unsafeCheck(left.width, right.width)
//          val wc = DFUInt.op[wcW.Out](wcW(left.width, right.width), "+", left.getInit + right.getInit, left, right)
//          new AdderUInt[ncW.Out, wcW.Out](wc) {
//          }
//        }
//      }
//
//    implicit def evUInt[LW, RW](
//      implicit
//      ncW : AdderWidth.NC[LW, RW],
//      wcW : AdderWidth.WC[LW, RW],
//      check : `LW >= RW`.Check[LW, RW]
//    ) = createUInt[LW, DFUInt[RW], RW](t => t.right)
//
//    implicit def evNum[LW, R <: Int, RW](
//      implicit
//      bitsWidthOf : BitsWidthOf.IntAux[R, RW],
//      ncW : AdderWidth.NC[LW, RW],
//      wcW : AdderWidth.WC[LW, RW],
//      check : `LW >= RW`.Check[LW, RW]
//    ) = createUInt[LW, R, RW](t => DFUInt.const[RW](TokenUInt(bitsWidthOf(t.right), t.right)))
//  }
//
//}
//
//
//
//
