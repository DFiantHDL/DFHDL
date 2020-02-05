package ZFiant

import DFiant.internals._
import singleton.twoface._
import singleton.ops._

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Shift operations
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
protected abstract class OpsShift(op : DiSoOp.Shift) {
  @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Shift Ops with the type ${R}")
  trait Builder[L <: DFAny, R] extends DFAny.Op.Builder[L, R] {
    type Out = L
  }

  object Builder {
    object SmallShift extends Checked1Param.Int {
      type Cond[LW, RW] = BitsWidthOf.CalcInt[LW] >= RW
      type Msg[LW, RW] = "The shift vector is too large. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
      type ParamFace = Int
    }
    type Of[T <: DFAny.Type, W] = DFAny.Of[T]{type Width = W}
    def create[LW, RW](left : DFSInt[LW], right : DFUInt[RW])(
      implicit
      ctx : DFAny.Context,
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : DFSInt[LW] = {
      checkLWvRW.unsafeCheck(left.width, right.width)

      val out = left.dfType
      val func : (left.TToken, right.TToken) => out.TToken = op match {
        case DiSoOp.<< => _ << _
        case DiSoOp.>> =>_ >> _
      }
      DFAny.Func2(out, left, op, right)(func)
    }
    implicit def evDFSInt_op_DFUInt[LW, RW](
      implicit
      ctx : DFAny.Context,
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : Builder[DFSInt[LW], DFUInt[RW]] = (left, right) => create(left, right)

    implicit def evDFSInt_op_Const[LW, R <: Int, RW](
      implicit
      ctx : DFAny.Context,
      check : Natural.Int.CheckedShellSym[Builder[_,_], R],
      rConst : DFUInt.Const.PosOnly.Aux[Builder[_,_], R, RW],
      checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
    ) : Builder[DFSInt[LW], R] = (left, rightR) => {
      val right = rConst(rightR)
      create(left, right)
    }
  }
}
protected object OpsShift {
  trait Shiftable
  class Able[R](val value : R) extends DFAny.Op.Able[R]
  object Able {
    implicit class FromXInt[R <: XInt](right : R) extends Able[R](right)
    implicit class FromInt[R <: Int](right : R) extends Able[R](right)
    implicit class FromDFUInt[RW](right : DFUInt[RW]) extends Able[DFUInt[RW]](right)
  }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
