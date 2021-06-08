package DFiant.core

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall

final val ? = Bubble
