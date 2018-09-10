package DFiant.FunctionalLib

import DFiant._
import singleton.twoface._

object DFSIntOps {
  final class `Func2Comp+`[LW, RW, WCW] private (leftArg : DFSInt[LW], rightArg : DFSInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp+`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp+`[LW, RW, WCW], DFSInt[LW], DFSInt[RW]](leftArg, "+", rightArg)(wcw)(ctx, DFSInt) with DFSInt[WCW] {
    final protected val tokenFunc = (a, b) => a + b
  }
  object `Func2Comp+` {
    def apply[LW, RW, WCW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp+`[LW, RW, WCW]],
      wcw : DFSInt.`Op+`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp+`[LW, RW, WCW] = new `Func2Comp+`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp+`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp+`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp-`[LW, RW, WCW] private (leftArg : DFSInt[LW], rightArg : DFSInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp-`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp-`[LW, RW, WCW], DFSInt[LW], DFSInt[RW]](leftArg, "-", rightArg)(wcw)(ctx, DFSInt) with DFSInt[WCW] {
    final protected val tokenFunc = (a, b) => a - b
  }
  object `Func2Comp-` {
    def apply[LW, RW, WCW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp-`[LW, RW, WCW]],
      wcw : DFSInt.`Op-`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp-`[LW, RW, WCW] = new `Func2Comp-`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp-`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp-`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp*`[LW, RW, WCW] private (leftArg : DFSInt[LW], rightArg : DFSInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp*`[LW, RW, WCW]],
  ) extends Func2Comp[`Func2Comp*`[LW, RW, WCW], DFSInt[LW], DFSInt[RW]](leftArg, "*", rightArg)(wcw)(ctx, DFSInt) with DFSInt[WCW] {
    final protected val tokenFunc = (a, b) => a * b
  }
  object `Func2Comp*` {
    def apply[LW, RW, WCW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp*`[LW, RW, WCW]],
      wcw : DFSInt.`Op*`.Builder.Inference.WCW[LW, RW, WCW]
    ) : `Func2Comp*`[LW, RW, WCW] = new `Func2Comp*`[LW, RW, WCW](leftArg, rightArg)(wcw(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, WCW] : `Func2Comp*`[LW, RW, WCW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp*`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }


  final class `Func2Comp<<`[LW, RW] private (leftArg : DFSInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<<`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<<`[LW, RW], DFSInt[LW], DFUInt[RW]](leftArg, "<<", rightArg)(leftArg.width)(ctx, DFSInt) with DFSInt[LW] {
    final protected val tokenFunc = (a, b) => a << b
  }
  object `Func2Comp<<` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp<<`[LW, RW]]
    ) : `Func2Comp<<`[LW, RW] = new `Func2Comp<<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW, OW] : `Func2Comp<<`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp<<`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>>`[LW, RW] private (leftArg : DFSInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>>`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>>`[LW, RW], DFSInt[LW], DFUInt[RW]](leftArg, ">>", rightArg)(leftArg.width)(ctx, DFSInt) with DFSInt[LW] {
    final protected val tokenFunc = (a, b) => a >> b
  }
  object `Func2Comp>>` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp>>`[LW, RW]]
    ) : `Func2Comp>>`[LW, RW] = new `Func2Comp>>`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW, OW] : `Func2Comp>>`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp>>`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp==`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]],
  ) extends Func2Comp[`Func2Comp==`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, "==", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]])
    : `Func2Comp==`[LW, RW] = new `Func2Comp==`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp==`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp==`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp!=`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, "!=", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]])
    : `Func2Comp!=`[LW, RW] = new `Func2Comp!=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp!=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp!=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, "<", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a < b
  }
  object `Func2Comp<` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]])
    : `Func2Comp<`[LW, RW] = new `Func2Comp<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp<`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, ">", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a > b
  }
  object `Func2Comp>` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]])
    : `Func2Comp>`[LW, RW] = new `Func2Comp>`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp>`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<=`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<=`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, "<=", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a <= b
  }
  object `Func2Comp<=` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]])
    : `Func2Comp<=`[LW, RW] = new `Func2Comp<=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp<=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>=`[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>=`[LW, RW], DFSInt[LW], DFSInt[RW]](leftArg, ">=", rightArg)(1)(ctx, DFSInt) with DFBool {
    final protected val tokenFunc = (a, b) => a >= b
  }
  object `Func2Comp>=` {
    def apply[LW, RW](leftArg : DFSInt[LW], rightArg : DFSInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]])
    : `Func2Comp>=`[LW, RW] = new `Func2Comp>=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFSIntOps._
      val opInst = new DFiant.BasicLib.DFSIntOps.`Comp>=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

}
