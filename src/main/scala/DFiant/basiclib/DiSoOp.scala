package DFiant.basiclib

import DFiant._

//Dual Input, Single Output Operation
trait DiSoOp[Kind <: DiSoOp.Kind, Left <: DFAny, Right <: DFAny, Result <: DFAny]
  extends DFComponent[DiSoOp[Kind, Left, Right, Result]] {
  val left : Left <> IN
  val right : Right <> IN
  val result : Result <> OUT
}

object DiSoOp {
  trait Kind
  object Kind {
    trait + extends Kind
    trait - extends Kind
  }


}
