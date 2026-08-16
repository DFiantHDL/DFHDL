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
  sealed trait AssignableNB extends Assignable
  sealed trait Connectable
  sealed trait Initializable
  sealed trait Initialized
  sealed trait Port
  sealed trait PortIN extends Port
  sealed trait PortOUT extends Port, Assignable
  sealed trait PortOUT_NB extends PortOUT, AssignableNB
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
    // type-level marker for a non-blocking output argument, written `T <> OUT.NB` in a method
    // signature (the `<>` match type maps it to an `PortOUT_NB` value)
    sealed class NB extends scala.annotation.StaticAnnotation
    // the modifier the harness uses to build an `<> OUT.NB` formal port: a signal-class output
    private[core] object pNB
        extends DclPort[PortOUT_NB](IRModifier(IRModifier.OUT, IRModifier.NB))
  object INOUT extends DclPort[PortINOUT](IRModifier(IRModifier.INOUT, IRModifier.Ordinary))
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
  extension (modifier: ModifierAny) def asIR: IRModifier = modifier.value

  // A declaration needs a scope that can declare something: `HasVars` (a design, a domain, a
  // process, an `initial` block, or a method body) or `HasPorts` (those, plus an interface).
  // Neither is granted by `Scope.Global`, so a top-level declaration is still rejected.
  protected type DclScope[S <: DFC.Scope] = AssertGiven[
    S <:< DFC.Scope.HasVars | DFC.Scope.HasPorts,
    "Port/Variable declarations cannot be global"
  ]
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
      ck: SC,
      dt: DT
  )(using
      checkScope: DclScope[ck.type]
  ): ExactOp2Aux["<>", DFC, Any, T, M, DFVal[
    OT,
    Modifier[A & SC & DT, C, I, P]
  ]] = new ExactOp2["<>", DFC, Any, T, M]:
    type Out = DFVal[OT, Modifier[A & SC & DT, C, I, P]]
    def apply(t: T, modifier: M)(using DFC): Out = trydf {
      val ownerIR = dfc.owner.asIR
      // check that the owner is a domain
      if (modifier.value.isPort)
        ownerIR match
          case _: ir.DFDomainOwner =>
          case _                   =>
            throw new IllegalArgumentException(
              "Ports can only be directly owned by a design, a domain or an interface."
            )

      val dfType = tc(t)
      dfType.asIR match
        case ir.DFOpaque(kind = kind) => kind match
            case ir.DFOpaque.Kind.Clk | ir.DFOpaque.Kind.Rst =>
              ownerIR match
                case domainOwner: ir.DFDomainOwner =>
                  import dfc.getSet
                  domainOwner.meta.annotations.collectFirst {
                    case rel: ir.constraints.Timing.Related => rel.ref.get
                  } match
                    case Some(target) =>
                      kind match
                        // input/output clock ports are allowed: they declare a derived clock
                        // that is fully synchronous with the related domain's clock (e.g. a
                        // gated version of it), while the reset is still shared through the
                        // relation. An input port consumes the derived clock; an output port
                        // sources it (the gating site drives it from the design scope)
                        case ir.DFOpaque.Kind.Clk
                            if modifier.value.isPort &&
                              (modifier.value.dir == IRModifier.IN ||
                                modifier.value.dir == IRModifier.OUT) =>
                        case ir.DFOpaque.Kind.Clk =>
                          throw new IllegalArgumentException(
                            s"Only clock ports (`Clk <> IN` / `Clk <> OUT`) are allowed in a related domain.\nSuch a clock is derived from (fully synchronous with) the clock of the related domain `${target.getName}`, and is typically a gated version of it."
                          )
                        case _ =>
                          throw new IllegalArgumentException(
                            s"Cannot create a rst in a related domain.\nA related domain always shares the reset of its related domain `${target.getName}`. To opt out of the reset, use `@timing.related(..., includeReset = false)`."
                          )
                    case None =>
                  end match
                case _ =>
            case _ =>
        case _ =>
      end match
      DFVal.Dcl(dfType, modifier.asInstanceOf[Modifier[A & SC & DT, C, I, P]])
    }(using dfc, CTName("Port/Variable constructor"))
  end evPortVarConstructor
end Modifier

sealed trait VAL
// Type-level direction markers for HDL-method arguments (`T <> IN` / `T <> OUT`), distinct
// from the same-named value modifiers `Modifier.IN` / `Modifier.OUT` used to declare ports:
// term-vs-type context disambiguates the two. A procedural ED method takes its arguments as
// directional ports (an input the call reads, an output the call writes), so these appear only
// in a procedure's parameter list.
sealed class IN extends scala.annotation.StaticAnnotation
sealed class OUT extends scala.annotation.StaticAnnotation
sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
type NOTCONST = Any
sealed trait DFRET
sealed trait RTRET
sealed trait EDRET
sealed trait CONSTRET
