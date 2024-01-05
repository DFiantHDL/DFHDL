package dfhdl.compiler.patching
import dfhdl.core.*
import dfhdl.compiler.ir
import Patch.Add.Config as AddCfg
import scala.annotation.unchecked.uncheckedVariance

type MetaDesignAny = MetaDesign[DFC.Domain]
abstract class MetaDesign[+D <: DFC.Domain](
    positionMember: ir.DFMember,
    addCfg: AddCfg,
    domainType: D = DFC.Domain.DF
)(using
    getSet: ir.MemberGetSet
) extends Design
    with reflect.Selectable:
  lazy val patch = positionMember -> Patch.Add(this, addCfg)
  lazy val injectedOwner: ir.DFOwner = addCfg match
    case AddCfg.InsideFirst | AddCfg.InsideLast =>
      positionMember match
        case positionOwner: ir.DFOwner => positionOwner
        case _ => throw new IllegalArgumentException("Expecting owner member for AddInside config.")
    case _ => positionMember.getOwner
  final override private[dfhdl] def initOwner: Design.Block =
    dfc.mutableDB.addMember(injectedOwner)
    injectedOwner.getThisOrOwnerDesign.asFE
  injectedOwner match
    case design: ir.DFDesignBlock => // do nothing
    case _ =>
      dfc.enterOwner(injectedOwner.asFE)

  dfc.mutableDB.setMetaGetSet(getSet)
  final type TDomain = D @uncheckedVariance
  final protected given TDomain = domainType
  // we don't really care about the IR domain type of a meta design, since it is removed.
  // only the compile-time domain type is important for meta-programming with the relevant constraints.
  final lazy val __domainType: ir.DomainType = ir.DomainType.DF

  final def plantMember[T <: ir.DFMember](member: T): T =
    dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate erroneous designs
  final override private[dfhdl] def skipChecks: Boolean = true

  export dfhdl.hdl.{RTDomainCfg => _, ClkCfg => _, RstCfg => _, *}
  export dfhdl.core.{asValAny, asVarAny, asDclAny, asTokenAny, asTokenOf}
  extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized]]]
end MetaDesign
