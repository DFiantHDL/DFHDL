package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

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
  final private[core] def containedOwner: TOwner =
    DFOwner(dfc.mutableDB.OwnershipContext.containerizedOwnerOfRef(ownerRef)).asInstanceOf[TOwner]
  dfc.enterOwner(__initOwner)
end Container

abstract class DomainContainer[D <: DomainType](domainType: D) extends Container:
  private[core] type TDomain = D
  final protected given TDomain = domainType
  final private[core] lazy val __domainType: ir.DomainType = domainType.asIR

abstract class RTDomainContainer(cfg: RTDomainCfg) extends DomainContainer(DomainType.RT(cfg)):

  protected lazy val Clk: DFOpaque[DFOpaque.Clk] =
    case class Clk() extends DFOpaque.Clk
    cfg match
      case ir.RTDomainCfg.Related(ref) =>
        import dfc.getSet
        throw new IllegalArgumentException(
          s"Cannot create an explicit clock in a related domain.\nYou can create the clock in the primary domain `${ref.get.getName}` and reference it here instead."
        )
      case _ =>
    DFOpaque(Clk())
  end Clk

  protected lazy val Rst: DFOpaque[DFOpaque.Rst] =
    case class Rst() extends DFOpaque.Rst
    cfg match
      case ir.RTDomainCfg.Related(ref) =>
        import dfc.getSet
        throw new IllegalArgumentException(
          s"Cannot create an explicit reset in a related domain.\nYou can create the reset in the primary domain `${ref.get.getName}` and reference it here instead."
        )
      case _ =>
    DFOpaque(Rst())
  end Rst
end RTDomainContainer
