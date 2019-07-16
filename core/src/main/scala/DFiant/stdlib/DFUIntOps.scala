/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.stdlib
import DFiant._
import internals.devAccess
import singleton.ops._
import singleton.twoface._

object DFUIntOps {
  final class `Func2Comp+`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp+`[LW, RW, WCW]],
  ) extends DFFunc2[`Func2Comp+`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "+", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
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
      import targetLib.DFUIntOps._
      if (inLeft.isConstant && inLeft.constLB.get.value == 0) outResult.connectWith(inRight)
      else if (inRight.isConstant && inRight.constLB.get.value == 0) outResult.connectWith(inLeft)
      else {
        val opInst = new DFiant.targetlib.DFUIntOps.`Comp+`(inLeft.width, inRight.width, outResult.width)
        opInst.inLeft <> inLeft
        opInst.inRight <> inRight
        opInst.outResult <> outResult
      }
    }
  }

  final class `Func2Comp-`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp-`[LW, RW, WCW]],
  ) extends DFFunc2[`Func2Comp-`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "-", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
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
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp-`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp*`[LW, RW, WCW] private (leftArg : DFUInt[LW], rightArg : DFUInt[RW])(wcw : TwoFace.Int[WCW])(
    implicit ctx : DFComponent.Context[`Func2Comp*`[LW, RW, WCW]],
  ) extends DFFunc2[`Func2Comp*`[LW, RW, WCW], DFUInt[LW], DFUInt[RW]](leftArg, "*", rightArg)(wcw)(ctx, DFUInt) with DFUInt[WCW] {
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
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp*`(inLeft.width, inRight.width, outResult.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }


  final class `Func2Comp==`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]],
  ) extends DFFunc2[`Func2Comp==`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "==", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp==`[LW, RW]])
    : `Func2Comp==`[LW, RW] = new `Func2Comp==`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp==`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp==`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]],
  ) extends DFFunc2[`Func2Comp!=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "!=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp!=`[LW, RW]])
    : `Func2Comp!=`[LW, RW] = new `Func2Comp!=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp!=`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp!=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]],
  ) extends DFFunc2[`Func2Comp<`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "<", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a < b
  }
  object `Func2Comp<` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<`[LW, RW]])
    : `Func2Comp<`[LW, RW] = new `Func2Comp<`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp<`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]],
  ) extends DFFunc2[`Func2Comp>`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, ">", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a > b
  }
  object `Func2Comp>` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>`[LW, RW]])
    : `Func2Comp>`[LW, RW] = new `Func2Comp>`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp>`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp<=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]],
  ) extends DFFunc2[`Func2Comp<=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, "<=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a <= b
  }
  object `Func2Comp<=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp<=`[LW, RW]])
    : `Func2Comp<=`[LW, RW] = new `Func2Comp<=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp<=`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp<=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp>=`[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(
    implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]],
  ) extends DFFunc2[`Func2Comp>=`[LW, RW], DFUInt[LW], DFUInt[RW]](leftArg, ">=", rightArg)(1)(ctx, DFUInt) with DFBool {
    final protected val tokenFunc = (a, b) => a >= b
  }
  object `Func2Comp>=` {
    def apply[LW, RW](leftArg : DFUInt[LW], rightArg : DFUInt[RW])(implicit ctx : DFComponent.Context[`Func2Comp>=`[LW, RW]])
    : `Func2Comp>=`[LW, RW] = new `Func2Comp>=`[LW, RW](leftArg, rightArg)

    implicit def evImpl[LW, RW] : `Func2Comp>=`[LW, RW] => Unit = ifc => {
      import ifc._
      import targetLib.DFUIntOps._
      val opInst = new DFiant.targetlib.DFUIntOps.`Comp>=`(inLeft.width, inRight.width)
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

}
