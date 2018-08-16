package DFiant.basiclib
import DFiant._

object DFBoolOps {
  class BoolopBool[Kind <: DiSoOp.Kind](
    implicit ctx : DFComponent.Context[BoolopBool[Kind]], kind : Kind
  ) extends DFComponent[BoolopBool[Kind]] {
    final val inLeft = DFBool() <> IN
    final val inRight = DFBool() <> IN
    final val outResult = DFBool() <> OUT
  }

  type `Comp||` = BoolopBool[DiSoOp.Kind.||]
  type `Comp&&` = BoolopBool[DiSoOp.Kind.&&]
  type `Comp==` = BoolopBool[DiSoOp.Kind.==]
  type `Comp!=` = BoolopBool[DiSoOp.Kind.!=]
}
