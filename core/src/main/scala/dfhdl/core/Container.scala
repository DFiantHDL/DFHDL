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

abstract class DomainContainer[D <: DomainType](domainType: D) extends Container:
  private[core] type TDomain = D
  final protected given TDomain = domainType
  final private[core] lazy val __domainType: ir.DomainType = domainType.asIR

abstract class RTDomainContainer extends DomainContainer(DomainType.RT):
  final case class Clk() extends DFOpaque.Clk
  final case class Rst() extends DFOpaque.Rst
end RTDomainContainer

// Common ancestor of `Design` and `Interface`: marks a container whose
// `<> CONST` constructor parameters are turned into design-parameter members
// (`DFVal.DesignParam`) by the compiler plugin. The plugin keys body-parameter
// generation on this trait rather than on `Design` specifically, so interfaces
// (which are not `Design`s) get the same treatment.
trait HasConstParams
