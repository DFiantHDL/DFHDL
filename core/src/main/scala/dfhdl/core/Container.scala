package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

private trait Container extends OnCreateEvents, HasDFC:
  type This <: Container
  final lazy val dfc: DFC = __dfc
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  private[core] type TScope <: DFC.Scope
  private[core] type TDomain <: DomainType
  private[core] type TOwner <: DFOwnerAny
  private[core] lazy val __domainType: ir.DomainType
  private var ownerOpt: Option[TOwner] = None
  final private[core] def setOwner(owner: TOwner): this.type =
    ownerOpt = Some(owner)
    this
  private[dfhdl] def initOwner: TOwner
  final private[core] def owner: TOwner =
    ownerOpt match
      case Some(owner) => owner
      case None =>
        val owner = initOwner
        ownerOpt = Some(owner)
        owner
  dfc.enterOwner(owner)
end Container

abstract class DomainContainer[D <: DomainType](domainType: D) extends Container:
  private[core] type TDomain = D
  final protected given TDomain = domainType
  final private[core] lazy val __domainType: ir.DomainType = domainType.asIR

abstract class RTDomainContainer(cfg: ir.RTDomainCfg) extends DomainContainer(DomainType.RT(cfg)):
  private lazy val derivedCfg: ir.RTDomainCfg =
    import dfc.getSet
    var derivedCfg: ir.RTDomainCfg = cfg
    var owner: ir.DFDomainOwner = dfc.owner.asIR.getThisOrOwnerDomain
    while (derivedCfg == ir.DerivedCfg && !owner.isTop)
      owner = owner.getOwnerDomain
      owner.domainType match
        case ir.DomainType.RT(cfg: ir.RTDomainCfg.Explicit) => derivedCfg = cfg
        case _                                              =>
    derivedCfg

  protected lazy val Clk: DFOpaque[DFOpaque.Clk] =
    case class Clk(cfgName: String) extends DFOpaque.Clk:
      override lazy val typeName: String = s"Clk_${cfgName}"
    val clkTFE = derivedCfg match
      case ir.DerivedCfg => RTDesign.Clk_main()
      case cfg: ir.RTDomainCfg.Explicit =>
        dfc.mutableDB.RTDomainCfgContext.getClkOpaque(cfg, Clk(cfg.name))
    DFOpaque(clkTFE)

  protected lazy val Rst: DFOpaque[DFOpaque.Rst] =
    case class Rst(cfgName: String) extends DFOpaque.Rst:
      override lazy val typeName: String = s"Rst_${cfgName}"
    val clkTFE = derivedCfg match
      case ir.DerivedCfg => RTDesign.Rst_main()
      case cfg: ir.RTDomainCfg.Explicit =>
        dfc.mutableDB.RTDomainCfgContext.getRstOpaque(cfg, Rst(cfg.name))
    DFOpaque(clkTFE)
end RTDomainContainer
