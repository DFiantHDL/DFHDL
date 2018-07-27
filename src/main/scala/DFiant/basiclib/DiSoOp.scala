package DFiant.basiclib

//Dual Input, Single Output Operation
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
