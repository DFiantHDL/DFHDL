package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import scala.reflect.ClassTag
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
    def apply(domainType: ir.DomainType)(using DFC): Block = trydf:
      dfc.owner.asIR match
        case _: ir.DFDomainOwner =>
        case _                   =>
          throw new IllegalArgumentException(
            "A domain can only be directly owned by a design, an interface, or another domain."
          )
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFMember.Empty.ref)
      ir.DomainBlock(domainType, ownerRef, dfc.getMeta, dfc.tags).addMember.asFE
  end Block
  extension [D <: Domain](domain: D)
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): D =
      import dfc.getSet
      domain.containedOwner.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
      domain
    infix def setName(name: String)(using dfc: DFC): D =
      import dfc.getSet
      domain.containedOwner.asIR
        .setMeta(m =>
          if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
          else m.setName(name)
        )
      domain
  end extension

end Domain

trait NoClkRstDomain extends Domain:
  protected inline def Clk: DFOpaque[DFOpaque.Clk] =
    compiletime.error("Clk/Rst declarations are not allowed in this domain.")
  protected inline def Rst: DFOpaque[DFOpaque.Rst] =
    compiletime.error("Clk/Rst declarations are not allowed in this domain.")

abstract class DFDomain extends DomainContainer(DomainType.DF), NoClkRstDomain

abstract class RTDomain(
    cfg: RTDomainCfg = RTDomainCfg.Derived
) extends RTDomainContainer(cfg),
      Domain:
  related =>
  abstract class RelatedDomain extends RTDomain(RTDomainCfg.Related(related))

abstract class EDDomain extends DomainContainer(DomainType.ED), NoClkRstDomain
