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
  sealed trait RegRef
  sealed trait VarRef
  sealed trait WireRef
  type VAR = Modifier[Assignable & VarRef, Connectable & VarRef, Initializable]
  final val VAR = new VAR(ir.DFVal.Modifier.VAR)
  type Port = Modifier[Assignable, Connectable, Initializable]
  final val IN = new Port(ir.DFVal.Modifier.IN)
  final val OUT = new Port(ir.DFVal.Modifier.OUT)
  final val INOUT = new Port(ir.DFVal.Modifier.INOUT)

  extension (modifier: ModifierAny) def asIR: ir.DFVal.Modifier = modifier.value
end Modifier

final type VAL = Modifier[Any, Any, Any]
sealed trait RET

val OPEN = Exact(ir.OpenConnectTag)
