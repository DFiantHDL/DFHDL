package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

private[DFiant] abstract class Domain(using DFC) extends Container with scala.reflect.Selectable:
  private[core] type TScope = Container.Scope.Domain
  final protected given TScope = Container.Scope.Domain
  private[core] final override lazy val owner: Domain.Block =
    Domain.Block(__domainType)

object Domain:
  type Block = DFOwner[ir.DomainBlock]
  object Block:
    def apply(domainType: ir.DomainType)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DomainBlock(
        domainType,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
end Domain

abstract class DFDomain(using DFC) extends Domain:
  private[core] type TDomain = Container.Domain.DF
  final protected given TDomain = Container.Domain.DF
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.DF

abstract class RTDomain(
    cfg: ir.RTDomainCfg = ir.DerivedCfg
)(using DFC)
    extends Domain:
  private[core] type TDomain = Container.Domain.RT
  final protected given TDomain = Container.Domain.RT
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.RT(cfg)

abstract class EDDomain(using DFC) extends Domain:
  private[core] type TDomain = Container.Domain.ED
  final protected given TDomain = Container.Domain.ED
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.ED
