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
import singleton.twoface._

object DFEnumOps {
  final class `Func2Comp==`[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[E]],
  ) extends DFFunc2[`Func2Comp==`[E], DFEnum[E], DFEnum[E]](leftArg, "==", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected lazy val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(implicit ctx : DFComponent.Context[`Func2Comp==`[E]])
    : `Func2Comp==`[E] = new `Func2Comp==`[E](leftArg, rightArg)

    implicit def evImpl[E <: Enum] : `Func2Comp==`[E] => Unit = ifc => {
      import ifc._
//      import targetLib.DFBitsOps._
//      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp==`(inLeft.width, inRight.width)
//      opInst.inLeft <> inLeft
//      opInst.inRight <> inRight
//      opInst.outResult <> outResult
      ???
    }
  }

  final class `Func2Comp!=`[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[E]],
  ) extends DFFunc2[`Func2Comp!=`[E], DFEnum[E], DFEnum[E]](leftArg, "!=", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected lazy val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(implicit ctx : DFComponent.Context[`Func2Comp!=`[E]])
    : `Func2Comp!=`[E] = new `Func2Comp!=`[E](leftArg, rightArg)

    implicit def evImpl[E <: Enum] : `Func2Comp!=`[E] => Unit = ifc => {
      import ifc._
      //      import targetLib.DFBitsOps._
      //      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp!=`(inLeft.width, inRight.width)
      //      opInst.inLeft <> inLeft
      //      opInst.inRight <> inRight
      //      opInst.outResult <> outResult
      ???
    }
  }


}
