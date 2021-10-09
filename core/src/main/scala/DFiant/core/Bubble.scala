package DFiant.core
import DFiant.compiler.ir

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour derives CanEqual:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def apply[T <: DFType](dfType: T): DFToken.Of[T] =
    ir.DFType.Token.bubble(dfType.asIR).asTokenOf[T]

final val ? = Bubble
