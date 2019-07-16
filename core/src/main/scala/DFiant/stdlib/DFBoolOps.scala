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
import singleton.ops._
import singleton.twoface._

object DFBoolOps {
  final class `Func2Comp==`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp==`],
  ) extends DFFunc2[`Func2Comp==`, DFBool, DFBool](leftArg, "==", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp==`])
    : `Func2Comp==` = new `Func2Comp==`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp==` => Unit = ifc => {
      import ifc._
      import targetLib.DFBoolOps._
      val opInst = new DFiant.targetlib.DFBoolOps.`Comp==`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp!=`],
  ) extends DFFunc2[`Func2Comp!=`, DFBool, DFBool](leftArg, "!=", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp!=`])
    : `Func2Comp!=` = new `Func2Comp!=`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp!=` => Unit = ifc => {
      import ifc._
      import targetLib.DFBoolOps._
      val opInst = new DFiant.targetlib.DFBoolOps.`Comp!=`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }
  

  final class `Func2Comp&&`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp&&`],
  ) extends DFFunc2[`Func2Comp&&`, DFBool, DFBool](leftArg, "&&", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a && b
  }
  object `Func2Comp&&` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp&&`])
    : `Func2Comp&&` = new `Func2Comp&&`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp&&` => Unit = ifc => {
      import ifc._
      import targetLib.DFBoolOps._
      val opInst = new DFiant.targetlib.DFBoolOps.`Comp&&`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp||`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp||`],
  ) extends DFFunc2[`Func2Comp||`, DFBool, DFBool](leftArg, "||", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a || b
  }
  object `Func2Comp||` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp||`])
    : `Func2Comp||` = new `Func2Comp||`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp||` => Unit = ifc => {
      import ifc._
      import targetLib.DFBoolOps._
      val opInst = new DFiant.targetlib.DFBoolOps.`Comp||`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp^`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp^`],
  ) extends DFFunc2[`Func2Comp^`, DFBool, DFBool](leftArg, "^", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a ^ b
  }
  object `Func2Comp^` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp^`])
    : `Func2Comp^` = new `Func2Comp^`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp^` => Unit = ifc => {
      import ifc._
      import targetLib.DFBoolOps._
      val opInst = new DFiant.targetlib.DFBoolOps.`Comp^`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

}
