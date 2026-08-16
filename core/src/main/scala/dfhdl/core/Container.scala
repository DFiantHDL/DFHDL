package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.platforms.resources.*

private trait Container extends OnCreateEvents, HasDFC, Wait.ContainerOps:
  type This <: Container
  final lazy val dfc: DFC = __dfc
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  private[core] type TScope <: DFC.Scope
  private[core] type TDomain <: DomainType
  private[core] type TOwner <: DFOwnerAny
  private[core] lazy val __domainType: ir.DomainType
  private[dfhdl] def initOwner: TOwner
  private val __initOwner = initOwner
  private val ownerRef: ir.DFRefAny = __initOwner.asIR.ownerRef
  final private[dfhdl] def containedOwner: TOwner =
    DFOwner(dfc.mutableDB.OwnershipContext.containerizedOwnerOfRef(ownerRef)).asInstanceOf[TOwner]
  dfc.enterOwner(__initOwner)
end Container

sealed trait DomainContainer extends Container

trait DFDomainContainer extends DomainContainer:
  private[core] type TDomain = DomainType.DF
  final protected given TDomain = DomainType.DF
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.DF

trait EDDomainContainer extends DomainContainer:
  private[core] type TDomain = DomainType.ED
  final protected given TDomain = DomainType.ED
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.ED

trait RTDomainContainer extends DomainContainer:
  private[core] type TDomain = DomainType.RT
  final protected given TDomain = DomainType.RT
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.RT
  final case class Clk() extends DFOpaque.Clk
  final case class Rst() extends DFOpaque.Rst
  // A domain related to its enclosing container, sharing its clock and reset: shorthand for
  // annotating the domain with `@timing.related(this)` of the enclosing container. The
  // annotation is injected at construction (before any subclass body member elaborates), so
  // it manifests exactly like a plain annotated `RTDomain`.
  abstract class RTRelatedDomain extends RTDomain:
    locally {
      import dfc.getSet
      val relatedAnnot = dfhdl.hw.constraints.timing.related(RTDomainContainer.this)(using dfc)
      containedOwner.asIR.setMeta(m => m.copy(annotations = m.annotations :+ relatedAnnot.asIR))
    }
  // A related domain with its own derived clock port (typically driven by a gated version of
  // the enclosing container's clock): shorthand for an `RTRelatedDomain` with an explicit
  // `val clk = Clk <> IN` declaration.
  abstract class RTDerivedClkDomain extends RTRelatedDomain:
    val clk = DFVal.Dcl(DFOpaque(Clk()), Modifier.IN)(using dfc.setName("clk"))
  // A scoping construct rather than a domain in its own right: a region groups logic under
  // this container's timing context with no observable footprint, neither a clock identity
  // nor a naming one (its members keep their bare names). Equivalent to an `RTRelatedDomain`
  // additionally annotated with `@flattenMode.transparent`. Typically used path-prefixed,
  // opening sparse regions of a domain declared once: `new active.RTRegion: <logic>`.
  abstract class RTRegion extends RTRelatedDomain:
    locally {
      import dfc.getSet
      containedOwner.asIR.setMeta(m =>
        m.copy(annotations = m.annotations :+ ir.annotation.FlattenMode.Transparent)
      )
    }
end RTDomainContainer
