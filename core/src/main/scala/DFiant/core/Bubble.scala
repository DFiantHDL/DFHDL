package DFiant.core
import DFiant.compiler.ir

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def apply[T <: DFType](dfType: T): DFToken.Of[T] =
    ir.DFType.Token.bubble(dfType.asIR).asInstanceOf[DFToken.Of[T]]

final val ? = Bubble
