package dfhdl.core
import dfhdl.compiler.ir

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour derives CanEqual:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def apply[T <: DFTypeAny](dfType: T): DFToken[T] =
    ir.DFToken.bubble(dfType.asIR).asTokenOf[T]

final val ? = Bubble
