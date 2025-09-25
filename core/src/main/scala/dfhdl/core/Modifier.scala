package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Modifier as IRModifier
import dfhdl.internals.*

//A: Access, C: Connectivity, I: Initialization, P: Parameteric (Const or not)
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
  sealed trait Port
  sealed trait PortIN extends Port
  sealed trait PortOUT extends Port, Assignable
  sealed trait PortINOUT extends PortIN, PortOUT
  type Mutable = Modifier[Assignable, Any, Any, dfhdl.core.NOTCONST]
  type Dcl = Modifier[Assignable, Connectable, Initializable, dfhdl.core.NOTCONST]
  type DclPort[+A] = Modifier[A, Connectable, Initializable, dfhdl.core.NOTCONST]
  type DclREG[+A] = Modifier[AssignableREG & A, Connectable, Initializable, dfhdl.core.NOTCONST]
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
    protected object pREG extends DclREG[Any](IRModifier(IRModifier.VAR, IRModifier.REG))
    inline def REG(using dt: DomainType)(using RTDomainOnly[dt.type]) = pREG
    protected object pSHARED extends DclSHARED(IRModifier(IRModifier.VAR, IRModifier.SHARED))
    inline def SHARED(using dt: DomainType)(using EDDomainOnly[dt.type]) = pSHARED
  object IN extends DclPort[PortIN](IRModifier(IRModifier.IN, IRModifier.Ordinary))
  object OUT extends DclPort[PortOUT](IRModifier(IRModifier.OUT, IRModifier.Ordinary)):
    protected object pREG extends DclREG[PortOUT](IRModifier(IRModifier.OUT, IRModifier.REG))
    inline def REG(using dt: DomainType)(using RTDomainOnly[dt.type]) = pREG
  object INOUT extends DclPort[PortINOUT](IRModifier(IRModifier.INOUT, IRModifier.Ordinary))
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
  extension (modifier: ModifierAny) def asIR: IRModifier = modifier.value

  given evPortVarConstructor[
      T <: DFType.Supported,
      OT <: DFTypeAny,
      A,
      C,
      I,
      P,
      SC <: DFC.Scope,
      DT <: DomainType,
      M <: Modifier[A, C, I, P]
  ](using
      tc: DFType.TC.Aux[T, OT],
      checkLocal: AssertGiven[DFC.Scope.Local, "Port/Variable declarations cannot be global"],
      ck: SC,
      dt: DT
  ): ExactOp2Aux["<>", DFC, Any, T, M, DFVal[
    OT,
    Modifier[A & SC & DT, C, I, P]
  ]] = new ExactOp2["<>", DFC, Any, T, M]:
    type Out = DFVal[OT, Modifier[A & SC & DT, C, I, P]]
    def apply(t: T, modifier: M)(using DFC): Out = trydf {
      if (modifier.value.isPort)
        dfc.owner.asIR match
          case _: ir.DFDomainOwner =>
          case _                   =>
            throw new IllegalArgumentException(
              "Ports can only be directly owned by a design, a domain or an interface."
            )
      DFVal.Dcl(tc(t), modifier.asInstanceOf[Modifier[A & SC & DT, C, I, P]])
    }(using dfc, CTName("Port/Variable constructor"))
  end evPortVarConstructor
end Modifier

sealed trait VAL
sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
type NOTCONST = Any
sealed trait DFRET
sealed trait RTRET
sealed trait EDRET
