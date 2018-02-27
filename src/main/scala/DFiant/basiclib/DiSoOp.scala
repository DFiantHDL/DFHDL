package DFiant.basiclib

import DFiant._

//Dual Input, Single Output Operation
trait DiSoOp[Comp[L <: DFAny, R <: DFAny, Res <: DFAny] <: core.DFDesign, Left <: DFAny, Right <: DFAny, Result <: DFAny]
  extends DFComponent[Comp[Left, Right, Result]] {
  val left : Left <> IN
  val right : Right <> IN
  val result : Result <> OUT
}

object DiSoOp {

  trait +[Left <: DFAny, Right <: DFAny, Result <: DFAny] extends DiSoOp[+, Left, Right, Result]


}
