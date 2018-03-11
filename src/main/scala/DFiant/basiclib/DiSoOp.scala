package DFiant.basiclib

import DFiant._

//Dual Input, Single Output Operation
trait DiSoOp[Kind <: DiSoOp.Kind, Left <: DFAny, Right <: DFAny, Result <: DFAny]
  extends DFComponent[DiSoOp[Kind, Left, Right, Result]] {
  val inLeft : Left <> IN
  val inRight : Right <> IN
  val outResult : Result <> OUT
}

object DiSoOp {
  trait Kind
  object Kind {
    trait +  extends Kind
    trait -  extends Kind
    trait *  extends Kind
    trait == extends Kind
    trait != extends Kind
    trait <  extends Kind
    trait >  extends Kind
    trait <= extends Kind
    trait >= extends Kind
  }


}
