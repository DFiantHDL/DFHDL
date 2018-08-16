package DFiant.basiclib
import DFiant._

object DFEnumOps {
  class EopEeqB[Kind <: DiSoOp.Kind, E <: Enum](e : E)(
    implicit ctx : DFComponent.Context[EopEeqB[Kind, E]]
  ) extends DFComponent[EopEeqB[Kind, E]] {
    final lazy val inLeft = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val inRight = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val outResult = ??? //DFBool() <> OUT
  }
  type `Comp==`[E <: Enum] = EopEeqB[DiSoOp.Kind.==, E]
  type `Comp!=`[E <: Enum] = EopEeqB[DiSoOp.Kind.!=, E]

}
