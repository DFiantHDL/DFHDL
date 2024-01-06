package dfhdl.core
import dfhdl.compiler.ir.DFVal.Modifier as IRModifier
import dfhdl.internals.*

sealed class Modifier[+A, +C, +I, +P](val value: IRModifier):
  override def toString: String = value.toString

type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  sealed trait Assignable
  sealed trait Connectable
  sealed trait Initializable
  sealed trait Initialized
  type Mutable = Modifier[Assignable, Any, Any, Any]
  type Dcl = Modifier[Assignable, Connectable, Initializable, Any]
  final val VAR = new Dcl(IRModifier.VAR)
  final val IN = new Dcl(IRModifier.IN)
  final val OUT = new Dcl(IRModifier.OUT)
  final val INOUT = new Dcl(IRModifier.INOUT)
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
  extension (modifier: ModifierAny) def asIR: IRModifier = modifier.value
end Modifier

final type VAL = Modifier[Any, Any, Any, Any]
sealed trait CONST
sealed trait DFRET

object __OPEN
val OPEN = Exact(__OPEN)
