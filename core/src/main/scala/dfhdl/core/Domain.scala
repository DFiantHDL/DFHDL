package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*

private[dfhdl] abstract class Domain(using DFC) extends Container with scala.reflect.Selectable:
  private[core] type TScope = DFC.Scope.Domain
  private[core] type TOwner = Domain.Block
  final protected given TScope = DFC.Scope.Domain
  final private[core] def initOwner: TOwner =
    Domain.Block(__domainType)
  final override def onCreateEnd: Unit =
    dfc.exitOwner()

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
  private[core] type TDomain = DFC.Domain.DF
  final protected given TDomain = DFC.Domain.DF
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.DF

abstract class RTDomain(
    cfg: ir.RTDomainCfg = ir.DerivedCfg
)(using DFC)
    extends Domain:
  private[core] type TDomain = DFC.Domain.RT
  final protected given TDomain = DFC.Domain.RT
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.RT(cfg)

abstract class EDDomain(using DFC) extends Domain:
  private[core] type TDomain = DFC.Domain.ED
  final protected given TDomain = DFC.Domain.ED
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.ED
