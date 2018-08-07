package DFiant.basiclib

//Dual Input, Single Output Operation
object DiSoOp {
  sealed trait Kind
  object Kind {
    sealed trait +  extends Kind
    sealed trait -  extends Kind
    sealed trait *  extends Kind
    sealed trait == extends Kind
    sealed trait != extends Kind
    sealed trait <  extends Kind
    sealed trait >  extends Kind
    sealed trait <= extends Kind
    sealed trait >= extends Kind
    case object +  extends +
    case object -  extends -
    case object *  extends *
    case object == extends ==
    case object != extends !=
    case object <  extends <
    case object >  extends >
    case object <= extends <=
    case object >= extends >=
  }


}
