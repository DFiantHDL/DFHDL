package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*

private[dfhdl] trait Domain extends Container with scala.reflect.Selectable:
  private[core] type TScope = DFC.Scope.Domain
  private[core] type TOwner = Domain.Block
  final protected given TScope = DFC.Scope.Domain
  final private[dfhdl] def initOwner: TOwner =
    Domain.Block(__domainType)
  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    dfc.exitOwner()

object Domain:
  type Block = DFOwner[ir.DomainBlock]
  object Block:
    def apply(domainType: ir.DomainType)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFMember.Empty.ref)
      ir.DomainBlock(
        domainType,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
end Domain

abstract class DFDomain extends DomainContainer(DomainType.DF), Domain

abstract class RTDomain(cfg: ir.RTDomainCfg = ir.DerivedCfg) extends RTDomainContainer(cfg), Domain

abstract class EDDomain extends DomainContainer(DomainType.ED), Domain
