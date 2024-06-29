package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import ir.FlattenMode
private[dfhdl] trait Domain extends Container with scala.reflect.Selectable:
  private[core] type TScope = DFC.Scope.Domain
  private[core] type TOwner = Domain.Block
  final protected given TScope = DFC.Scope.Domain
  private[core] lazy val __flattenMode: FlattenMode
  final private[dfhdl] def initOwner: TOwner =
    Domain.Block(__domainType, __flattenMode)
  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    dfc.exitOwner()

object Domain:
  type Block = DFOwner[ir.DomainBlock]
  object Block:
    def apply(domainType: ir.DomainType, flattenMode: ir.FlattenMode)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFMember.Empty.ref)
      ir.DomainBlock(
        domainType, flattenMode, ownerRef, dfc.getMeta, dfc.tags
      ).addMember.asFE
end Domain

abstract class DFDomain(flattenMode: FlattenMode = FlattenMode.FlattenUnderscore)
    extends DomainContainer(DomainType.DF),
      Domain:
  final private[core] lazy val __flattenMode: FlattenMode = flattenMode

abstract class RTDomain(
    cfg: ir.RTDomainCfg = ir.DerivedCfg,
    flattenMode: FlattenMode = FlattenMode.FlattenUnderscore
) extends RTDomainContainer(cfg),
      Domain:
  final private[core] lazy val __flattenMode: FlattenMode = flattenMode

abstract class EDDomain(flattenMode: FlattenMode = FlattenMode.FlattenUnderscore)
    extends DomainContainer(DomainType.ED),
      Domain:
  final private[core] lazy val __flattenMode: FlattenMode = flattenMode
