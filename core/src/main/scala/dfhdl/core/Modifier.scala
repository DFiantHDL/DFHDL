package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

sealed class Modifier[+A, +C, +I](val value: ir.DFVal.Modifier):
  override def toString: String = value.toString

type ModifierAny = Modifier[Any, Any, Any]
object Modifier:
  sealed trait Assignable
  sealed trait Connectable
  sealed trait Initializable
  sealed trait Initialized
  type Mutable = Modifier[Assignable, Connectable, Initializable]
  final val VAR = new Mutable(ir.DFVal.Modifier.VAR)
  final val IN = new Mutable(ir.DFVal.Modifier.IN)
  final val OUT = new Mutable(ir.DFVal.Modifier.OUT)
  final val INOUT = new Mutable(ir.DFVal.Modifier.INOUT)

  extension (modifier: ModifierAny) def asIR: ir.DFVal.Modifier = modifier.value
end Modifier

final type VAL = Modifier[Any, Any, Any]
sealed trait DFRET

object __OPEN
val OPEN = Exact(__OPEN)
