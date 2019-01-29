package DFiant.FunctionalLib

import DFiant._
import singleton.ops._
import singleton.twoface._

object DFBoolOps {
  final class `Func2Comp==`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp==`],
  ) extends Func2Comp[`Func2Comp==`, DFBool, DFBool](leftArg, "==", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a == b
  }
  object `Func2Comp==` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp==`])
    : `Func2Comp==` = new `Func2Comp==`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp==` => Unit = ifc => {
      import ifc._
      import basicLib.DFBoolOps._
      val opInst = new DFiant.BasicLib.DFBoolOps.`Comp==`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp!=`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp!=`],
  ) extends Func2Comp[`Func2Comp!=`, DFBool, DFBool](leftArg, "!=", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a != b
  }
  object `Func2Comp!=` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp!=`])
    : `Func2Comp!=` = new `Func2Comp!=`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp!=` => Unit = ifc => {
      import ifc._
      import basicLib.DFBoolOps._
      val opInst = new DFiant.BasicLib.DFBoolOps.`Comp!=`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }
  

  final class `Func2Comp&&`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp&&`],
  ) extends Func2Comp[`Func2Comp&&`, DFBool, DFBool](leftArg, "&&", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a && b
  }
  object `Func2Comp&&` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp&&`])
    : `Func2Comp&&` = new `Func2Comp&&`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp&&` => Unit = ifc => {
      import ifc._
      import basicLib.DFBoolOps._
      val opInst = new DFiant.BasicLib.DFBoolOps.`Comp&&`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp||`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp||`],
  ) extends Func2Comp[`Func2Comp||`, DFBool, DFBool](leftArg, "||", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a || b
  }
  object `Func2Comp||` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp||`])
    : `Func2Comp||` = new `Func2Comp||`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp||` => Unit = ifc => {
      import ifc._
      import basicLib.DFBoolOps._
      val opInst = new DFiant.BasicLib.DFBoolOps.`Comp||`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

  final class `Func2Comp^`(leftArg : DFBool, rightArg : DFBool)(
    implicit ctx : DFComponent.Context[`Func2Comp^`],
  ) extends Func2Comp[`Func2Comp^`, DFBool, DFBool](leftArg, "^", rightArg)(1)(ctx, DFBool) with DFBool {
    final protected val tokenFunc = (a, b) => a ^ b
  }
  object `Func2Comp^` {
    def apply(leftArg : DFBool, rightArg : DFBool)(implicit ctx : DFComponent.Context[`Func2Comp^`])
    : `Func2Comp^` = new `Func2Comp^`(leftArg, rightArg)

    implicit def evImpl : `Func2Comp^` => Unit = ifc => {
      import ifc._
      import basicLib.DFBoolOps._
      val opInst = new DFiant.BasicLib.DFBoolOps.`Comp^`
      opInst.inLeft <> inLeft
      opInst.inRight <> inRight
      opInst.outResult <> outResult
    }
  }

}
