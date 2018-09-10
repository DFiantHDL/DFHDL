package DFiant.FunctionalLib

import DFiant._
import singleton.twoface._

object DFBitsOps {
  final class `Func2Comp&`[LW, RW, OW] private (leftArg : DFBits[LW], rightArg : DFBits[RW])(wcw : TwoFace.Int[OW])(
    implicit ctx : DFComponent.Context[`Func2Comp&`[LW, RW, OW]],
  ) extends Func2Comp[`Func2Comp&`[LW, RW, OW], DFBits[LW], DFBits[RW]](leftArg, "&", rightArg)(wcw)(ctx, DFBits) with DFBits[OW] {
    final protected val tokenFunc = (a, b) => a & b
  }
  object `Func2Comp&` {
    def apply[LW, RW, OW](leftArg : DFBits[LW], rightArg : DFBits[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp&`[LW, RW, OW]],
      oW : DFBits.`Op&`.Builder.Inference.OW[LW, RW, OW]
    ) : `Func2Comp&`[LW, RW, OW] = new `Func2Comp&`[LW, RW, OW](leftArg, rightArg)(oW(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, OW] : `Func2Comp&`[LW, RW, OW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp&`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp|`[LW, RW, OW] private (leftArg : DFBits[LW], rightArg : DFBits[RW])(wcw : TwoFace.Int[OW])(
    implicit ctx : DFComponent.Context[`Func2Comp|`[LW, RW, OW]],
  ) extends Func2Comp[`Func2Comp|`[LW, RW, OW], DFBits[LW], DFBits[RW]](leftArg, "|", rightArg)(wcw)(ctx, DFBits) with DFBits[OW] {
    final protected val tokenFunc = (a, b) => a | b
  }
  object `Func2Comp|` {
    def apply[LW, RW, OW](leftArg : DFBits[LW], rightArg : DFBits[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp|`[LW, RW, OW]],
      oW : DFBits.`Op|`.Builder.Inference.OW[LW, RW, OW]
    ) : `Func2Comp|`[LW, RW, OW] = new `Func2Comp|`[LW, RW, OW](leftArg, rightArg)(oW(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, OW] : `Func2Comp|`[LW, RW, OW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp|`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp^`[LW, RW, OW] private (leftArg : DFBits[LW], rightArg : DFBits[RW])(wcw : TwoFace.Int[OW])(
    implicit ctx : DFComponent.Context[`Func2Comp^`[LW, RW, OW]],
  ) extends Func2Comp[`Func2Comp^`[LW, RW, OW], DFBits[LW], DFBits[RW]](leftArg, "^", rightArg)(wcw)(ctx, DFBits) with DFBits[OW] {
    final protected val tokenFunc = (a, b) => a ^ b
  }
  object `Func2Comp^` {
    def apply[LW, RW, OW](leftArg : DFBits[LW], rightArg : DFBits[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp^`[LW, RW, OW]],
      oW : DFBits.`Op^`.Builder.Inference.OW[LW, RW, OW]
    ) : `Func2Comp^`[LW, RW, OW] = new `Func2Comp^`[LW, RW, OW](leftArg, rightArg)(oW(leftArg.width, rightArg.width))

    implicit def evImpl[LW, RW, OW] : `Func2Comp^`[LW, RW, OW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp^`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<<`[LW, RW] private (leftArg : DFBits[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<<`[LW, RW]],
  ) extends Func2Comp[`Func2Comp<<`[LW, RW], DFBits[LW], DFUInt[RW]](leftArg, "<<", rightArg)(leftArg.width)(ctx, DFBits) with DFBits[LW] {
    final protected val tokenFunc = (a, b) => a << b
  }
  object `Func2Comp<<` {
    def apply[LW, RW](leftArg : DFBits[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp<<`[LW, RW]]
    ) : `Func2Comp<<`[LW, RW] = new `Func2Comp<<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW, OW] : `Func2Comp<<`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp<<`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>>`[LW, RW] private (leftArg : DFBits[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>>`[LW, RW]],
  ) extends Func2Comp[`Func2Comp>>`[LW, RW], DFBits[LW], DFUInt[RW]](leftArg, ">>", rightArg)(leftArg.width)(ctx, DFBits) with DFBits[LW] {
    final protected val tokenFunc = (a, b) => a >> b
  }
  object `Func2Comp>>` {
    def apply[LW, RW](leftArg : DFBits[LW], rightArg : DFUInt[RW])(
      implicit
      ctx : DFComponent.Context[`Func2Comp>>`[LW, RW]]
    ) : `Func2Comp>>`[LW, RW] = new `Func2Comp>>`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW, OW] : `Func2Comp>>`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp>>`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp==`[LW, RW](leftArg : DFBits[LW], rightArg : DFBits[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]],
  ) extends Func2Comp[`Func2Comp==`[LW, RW], DFBits[LW], DFBits[RW]](leftArg, "==", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[LW, RW](leftArg : DFBits[LW], rightArg : DFBits[RW])(implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]])
    : `Func2Comp==`[LW, RW] = new `Func2Comp==`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp==`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp==`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`[LW, RW](leftArg : DFBits[LW], rightArg : DFBits[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]],
  ) extends Func2Comp[`Func2Comp!=`[LW, RW], DFBits[LW], DFBits[RW]](leftArg, "!=", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[LW, RW](leftArg : DFBits[LW], rightArg : DFBits[RW])(implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]])
    : `Func2Comp!=`[LW, RW] = new `Func2Comp!=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp!=`[LW, RW] => Unit = ifc => {
      import ifc._
      import basicLib.DFBitsOps._
      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp!=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }


}
