package dfhdl.core
import dfhdl.compiler.ir.DFVal.Modifier as IRModifier
import dfhdl.internals.*

sealed class Modifier[+A, +C, +I, +P](val value: IRModifier):
  override def toString: String = value.toString

type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  sealed trait Assignable
  sealed trait AssignableREG
  sealed trait AssignableSHARED extends Assignable
  sealed trait Connectable
  sealed trait Initializable
  sealed trait Initialized
  type Mutable = Modifier[Assignable, Any, Any, dfhdl.core.NOTCONST]
  type Dcl = Modifier[Assignable, Connectable, Initializable, dfhdl.core.NOTCONST]
  type DclREG = Modifier[AssignableREG, Any, Initializable, dfhdl.core.NOTCONST]
  type DclSHARED = Modifier[AssignableSHARED, Any, Initializable, dfhdl.core.NOTCONST]
  protected type RTDomainOnly[A] = AssertGiven[
    A <:< DomainType.RT,
    "`.REG` declaration modifier is only allowed under register-transfer (RT) domains."
  ]
  protected type EDDomainOnly[A] = AssertGiven[
    A <:< DomainType.ED,
    "`.SHARED` variable modifier is only allowed under event-driven (ED) domains."
  ]
  object VAR extends Dcl(IRModifier(IRModifier.VAR, IRModifier.Ordinary)):
    protected object pREG extends DclREG(IRModifier(IRModifier.VAR, IRModifier.REG))
    inline def REG(using dt: DomainType)(using RTDomainOnly[dt.type]) = pREG
    protected object pSHARED extends DclSHARED(IRModifier(IRModifier.VAR, IRModifier.SHARED))
    inline def SHARED(using dt: DomainType)(using EDDomainOnly[dt.type]) = pSHARED
  object IN
      extends Modifier[Any, Connectable, Initializable, dfhdl.core.NOTCONST](
        IRModifier(IRModifier.IN, IRModifier.Ordinary)
      )
  object OUT extends Dcl(IRModifier(IRModifier.OUT, IRModifier.Ordinary)):
    protected object pREG extends DclREG(IRModifier(IRModifier.OUT, IRModifier.REG))
    inline def REG(using dt: DomainType)(using RTDomainOnly[dt.type]) = pREG
  object INOUT extends Dcl(IRModifier(IRModifier.INOUT, IRModifier.Ordinary)):
    protected object pREG extends DclREG(IRModifier(IRModifier.INOUT, IRModifier.REG))
    inline def REG(using dt: DomainType)(using RTDomainOnly[dt.type]) = pREG
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
  extension (modifier: ModifierAny) def asIR: IRModifier = modifier.value
end Modifier

sealed trait VAL
sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
type NOTCONST = Any
sealed trait DFRET

object r__OPEN
val OPEN = Exact(r__OPEN)
