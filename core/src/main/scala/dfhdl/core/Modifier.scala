package dfhdl.core
import dfhdl.compiler.ir.DFVal.Modifier as IRModifier
import dfhdl.internals.*

sealed class Modifier[+A, +C, +I, +P](val value: IRModifier):
  override def toString: String = value.toString

type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  sealed trait Assignable
  sealed trait AssignableREG
  sealed trait Connectable
  sealed trait Initializable
  sealed trait Initialized
  type Mutable = Modifier[Assignable, Any, Any, dfhdl.core.NOTCONST]
  type Dcl = Modifier[Assignable, Connectable, Initializable, dfhdl.core.NOTCONST]
  type DclREG = Modifier[AssignableREG, Any, Initializable, dfhdl.core.NOTCONST]
  object VAR extends Dcl(IRModifier(IRModifier.VAR, false)):
    object REG extends DclREG(IRModifier(IRModifier.VAR, true))
  object IN
      extends Modifier[Any, Connectable, Initializable, dfhdl.core.NOTCONST](
        IRModifier(IRModifier.IN, false)
      )
  object OUT extends Dcl(IRModifier(IRModifier.OUT, false)):
    object REG extends DclREG(IRModifier(IRModifier.OUT, true))
  object INOUT extends Dcl(IRModifier(IRModifier.INOUT, false)):
    object REG extends DclREG(IRModifier(IRModifier.INOUT, true))
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
  extension (modifier: ModifierAny) def asIR: IRModifier = modifier.value
end Modifier

sealed trait VAL
sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
type NOTCONST = Any
sealed trait DFRET

object __OPEN
val OPEN = Exact(__OPEN)
