package DFiant.FunctionalLib

import DFiant._
import singleton.twoface._

object DFEnumOps {
  final class `Func2Comp==`[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(
    implicit ctx : DFComponent.Context[`Func2Comp==`[E]],
  ) extends Func2Comp[`Func2Comp==`[E], DFEnum[E], DFEnum[E]](leftArg, "==", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(implicit ctx : DFComponent.Context[`Func2Comp==`[E]])
    : `Func2Comp==`[E] = new `Func2Comp==`[E](leftArg, rightArg)

    implicit def evImpl[E <: Enum] : `Func2Comp==`[E] => Unit = ifc => {
      import ifc._
//      import basicLib.DFBitsOps._
//      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp==`(inLeft.width, inRight.width)
//      opInst.inLeft <> inLeft
//      opInst.inRight <> inRight
//      opInst.outResult <> outResult
      ???
    }
  }

  final class `Func2Comp!=`[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(
    implicit ctx : DFComponent.Context[`Func2Comp!=`[E]],
  ) extends Func2Comp[`Func2Comp!=`[E], DFEnum[E], DFEnum[E]](leftArg, "!=", rightArg)(1)(ctx, DFBits) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply[E <: Enum](leftArg : DFEnum[E], rightArg : DFEnum[E])(implicit ctx : DFComponent.Context[`Func2Comp!=`[E]])
    : `Func2Comp!=`[E] = new `Func2Comp!=`[E](leftArg, rightArg)

    implicit def evImpl[E <: Enum] : `Func2Comp!=`[E] => Unit = ifc => {
      import ifc._
      //      import basicLib.DFBitsOps._
      //      val opInst = new DFiant.BasicLib.DFBitsOps.`Comp!=`(inLeft.width, inRight.width)
      //      opInst.inLeft <> inLeft
      //      opInst.inRight <> inRight
      //      opInst.outResult <> outResult
      ???
    }
  }


}
