package ZFiant

import DFiant.internals._
import singleton.twoface._
import singleton.ops._

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Shift operations
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
protected abstract class OpsShift[T[W] <: DFAny.Type{type Width = W}](op : DiSoOp.Shift) {
  def tokenFunc[LW, RW](left : T[LW]#TToken, right : DFUInt.Token[RW]) : T[LW]#TToken

  @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Shift Ops with the type ${R}")
  trait Builder[L <: DFAny, R] extends DFAny.Op.Builder[L, R] {
    type Out = L
  }

  object Builder {
    object SmallShift extends Checked1Param.Int {
      type Cond[LW, RW] = BitsWidthOf.CalcInt[LW-1] >= RW
      type Msg[LW, RW] = "The shift vector is too large. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
      type ParamFace = Int
    }
    def create[LW, RW](left : DFAny.Of[T[LW]], right : DFUInt[RW])(
      implicit
      ctx : DFAny.Context,
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : DFAny.Of[T[LW]] = {
      checkLWvRW.unsafeCheck(left.width, right.width)

      val out = left.dfType
      DFAny.Func2(out, left, op, right)(tokenFunc)
    }
    implicit def evDFSInt_op_DFUInt[LW, RW](
      implicit
      ctx : DFAny.Context,
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : Builder[DFAny.Of[T[LW]], DFUInt[RW]] = (left, right) => create(left, right)

    implicit def evDFSInt_op_Const[LW, R <: Int, RW](
      implicit
      ctx : DFAny.Context,
      rConst : DFUInt.Const.NatOnly.Aux[Builder[_,_], R, RW],
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : Builder[DFAny.Of[T[LW]], R] = (left, rightR) => create(left, rConst(rightR))
  }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
