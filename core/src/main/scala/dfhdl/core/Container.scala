package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

private abstract class Container extends OnCreateEvents, HasDFC:
  type This <: Container
  final lazy val dfc: DFC = __dfc
  protected def __dfc: DFC = DFC.empty
  private[core] type TScope <: DFC.Scope
  private[core] type TDomain <: DFC.Domain
  private[core] type TOwner <: DFOwnerAny
  private[core] lazy val __domainType: ir.DomainType
  private var ownerOpt: Option[TOwner] = None
  final private[core] def setOwner(owner: TOwner): this.type =
    ownerOpt = Some(owner)
    this
  private[core] def initOwner: TOwner
  final private[core] def owner: TOwner =
    ownerOpt match
      case Some(owner) => owner
      case None =>
        val owner = initOwner
        ownerOpt = Some(owner)
        owner
  dfc.enterOwner(owner)
end Container
