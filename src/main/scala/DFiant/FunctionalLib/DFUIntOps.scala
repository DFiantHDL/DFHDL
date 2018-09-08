package DFiant.FunctionalLib
import DFiant._
import singleton.ops._
import singleton.twoface._

object DFUIntOps {
  final class `Comp+`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Comp+`[LW, RW, WCW]],
  ) extends DiSoComp[`Comp+`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "+", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
    final protected val tokenFunc = (a, b) => a + b
  }
  object `Comp+` {
    object Inference {
      import singleton.ops.math.Max
      type CalcWCW[LW, RW] = Max[LW, RW] + 1
      type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
      type CalcNCW[LW, RW] = Max[LW, RW]
      type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
    }
    def apply[LW, RW, WCW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Comp+`[LW, RW, WCW]],
      wcw : Inference.WCW[LW, RW, WCW]
    ) : `Comp+`[LW, RW, WCW] = new `Comp+`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Comp+`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
    }
  }


  final class `Comp<`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Comp<`[LW, RW]],
  ) extends DiSoComp[`Comp<`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "<", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a < b
  }
  object `Comp<` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Comp<`[LW, RW]])
    : `Comp<`[LW, RW] = new `Comp<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Comp<`[LW, RW] => Unit = ifc => {
      import ifc._
    }

  }
}
