package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.ir.DomainType.RT
import DFiant.compiler.ir.DomainType.RT.{ClockParams, ResetParams}
import DFiant.compiler.printing.*

private[DFiant] abstract class Domain(using DFC) extends Container with scala.reflect.Selectable:
  private[core] type TKind = Container.Kind.Domain
  final protected given TKind = Container.Kind.Domain
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
  private[core] type TDomain = ir.DomainType.DF
  private[core] lazy val __domainType: TDomain = ir.DomainType.DF

abstract class RTDomain(
    clkParams: ClockParams = ClockParams(),
    rstParams: ResetParams = ResetParams()
)(using DFC)
    extends Domain:
  private[core] class TDomain extends ir.DomainType.RT.HL
  private[core] lazy val __domainType: TDomain = new TDomain
