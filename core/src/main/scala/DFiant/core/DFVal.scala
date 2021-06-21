package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFVal[+T <: DFType, +M <: DFVal.Modifier] = ir.DFVal
type DFValOf[+T <: DFType] = DFVal[T, DFVal.Modifier.VAL]
type DFVarOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Assignable]
type DFPortOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Port]

extension (dfVal: ir.DFVal)
  def asValOf[T <: DFType]: DFValOf[T] = dfVal
  def asVarOf[T <: DFType]: DFVarOf[T] = dfVal
  def asPortOf[T <: DFType]: DFPortOf[T] = dfVal

object DFVal:
  sealed trait Modifier extends Product, Serializable
  object Modifier:
    sealed trait Assignable extends Modifier
    sealed trait Connectable extends Modifier
    sealed trait VAL extends Modifier
    case object VAR extends VAL, Assignable, Connectable
    sealed trait Port extends VAL, Assignable, Connectable
    case object IN extends Port
    case object OUT extends Port
    case object INOUT extends Port

  extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
    def asIR: ir.DFVal = dfVal

  object Const:
    def apply[T <: DFType, D](token: DFToken.Of[T, D])(using DFC): DFValOf[T] =
      ir.DFVal.Const(token.asIR, ???, ???, ???)

end DFVal
