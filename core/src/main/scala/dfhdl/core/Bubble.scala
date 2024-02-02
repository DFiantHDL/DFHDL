package dfhdl.core
import dfhdl.compiler.ir

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour derives CanEqual:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def constValOf[T <: DFTypeAny](dfType: T, named: Boolean)(using DFC): DFConstOf[T] =
    DFVal.Const.forced(dfType, dfType.asIR.createBubbleData, named)

final val ? = Bubble
