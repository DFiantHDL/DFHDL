package DFiant.FunctionalLib
import DFiant._
import singleton.ops._
import singleton.twoface._

object DFUIntOps {
  final class `Func2Comp+`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp+`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp+`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "+", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
    final protected val tokenFunc = (a, b) => a + b
  }
  object `Func2Comp+` {
    def apply[LW, RW, WCW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp+`[LW, RW, WCW]],
      wcw : DFUInt.`Op+`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp+`[LW, RW, WCW] = new `Func2Comp+`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp+`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      if (inLeft.isConstant && inLeft.constLB.get.value == 0) outResult.connectPort2Port(inRight)
      else if (inRight.isConstant && inRight.constLB.get.value == 0) outResult.connectPort2Port(inLeft)
      else {
        val opInst = new DFiant.BasicLib.DFUIntOps.`Comp+`(inLeft.width, inRight.width, outResult.width)
        opInst.inLeft <> inLeft
        opInst.inRight <> inRight
        opInst.outResult <> outResult
      }
    }
  }

  final class `Func2Comp-`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp-`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp-`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "-", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
    final protected val tokenFunc = (a, b) => a - b
  }
  object `Func2Comp-` {
    def apply[LW, RW, WCW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp-`[LW, RW, WCW]],
      wcw : DFUInt.`Op-`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp-`[LW, RW, WCW] = new `Func2Comp-`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp-`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp-`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp*`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp*`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp*`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "*", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
    final protected val tokenFunc = (a, b) => a * b
  }
  object `Func2Comp*` {
    def apply[LW, RW, WCW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp*`[LW, RW, WCW]],
      wcw : DFUInt.`Op*`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp*`[LW, RW, WCW] = new `Func2Comp*`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp*`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp*`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }


  final class `Func2Comp==`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]],
  ) extends Func2Comp[`Func2Comp==`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "==", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]])
    : `Func2Comp==`[LW, RW] = new `Func2Comp==`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp==`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp==`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp!=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "!=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]])
    : `Func2Comp!=`[LW, RW] = new `Func2Comp!=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp!=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp!=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "<", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a < b
  }
  object `Func2Comp<` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]])
    : `Func2Comp<`[LW, RW] = new `Func2Comp<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp<`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, ">", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a > b
  }
  object `Func2Comp>` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]])
    : `Func2Comp>`[LW, RW] = new `Func2Comp>`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp>`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "<=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a <= b
  }
  object `Func2Comp<=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]])
    : `Func2Comp<=`[LW, RW] = new `Func2Comp<=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp<=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, ">=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a >= b
  }
  object `Func2Comp>=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]])
    : `Func2Comp>=`[LW, RW] = new `Func2Comp>=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFUIntOps._
      val opInst = new DFiant.BasicLib.DFUIntOps.`Comp>=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

}
