package dfhdl.compiler.patching
import dfhdl.core.*
import dfhdl.compiler.ir
import Patch.Add.Config as AddCfg
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.compiler.ir.DFDesignBlock

type MetaDesignAny = MetaDesign[DomainType]
abstract class MetaDesign[+D <: DomainType](
    positionMember: ir.DFMember,
    addCfg: AddCfg,
    domainType: D = DomainType.DF
)(using
    getSet: ir.MemberGetSet
) extends Design
    with reflect.Selectable:
  lazy val patch = positionMember -> Patch.Add(this, addCfg)
  private lazy val globalInjection: Boolean =
    (addCfg, positionMember) match
      case (AddCfg.Before, positionOwner: DFDesignBlock) if positionOwner.isTop => true
      case _                                                                    => false
  lazy val injectedOwner: ir.DFOwner = addCfg match
    case AddCfg.InsideFirst | AddCfg.InsideLast =>
      positionMember match
        case positionOwner: ir.DFOwner => positionOwner
        case _ => throw new IllegalArgumentException("Expecting owner member for AddInside config.")
    case _ if globalInjection => positionMember.asInstanceOf[DFDesignBlock]
    case _                    => positionMember.getOwner
  final override private[dfhdl] def initOwner: Design.Block =
    dfc.mutableDB.addMember(injectedOwner)
    injectedOwner.getThisOrOwnerDesign.asFE
  final override protected def __dfc: DFC = DFC.empty

  injectedOwner match
    case design: ir.DFDesignBlock => // do nothing
    case _ =>
      dfc.enterOwner(injectedOwner.asFE)

  dfc.mutableDB.injectMetaGetSet(getSet)
  final type TDomain = D @uncheckedVariance
  final protected given TDomain = domainType
  // we don't really care about the IR domain type of a meta design, since it is removed.
  // only the compile-time domain type is important for meta-programming with the relevant constraints.
  final lazy val __domainType: ir.DomainType = ir.DomainType.DF

  final def plantMember[T <: ir.DFMember](member: T): T =
    if (globalInjection)
      dfc.mutableDB.plantMember(ir.DFMember.Empty, member, _ => true)
    else
      dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def plantMembers(baseOwner: ir.DFOwner, members: Iterable[ir.DFMember]): Unit =
    members.foreach { m =>
      val owner = m.getOwner
      var cond = false
      val updatedOwner =
        if (owner == baseOwner)
          cond = true
          dfc.owner.asIR
        else owner
      dfc.mutableDB.plantMember(updatedOwner, m, _ => cond)
    }
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate erroneous designs
  final override private[dfhdl] def skipChecks: Boolean = true

  export dfhdl.hdl.{RTDomainCfg => _, ClkCfg => _, RstCfg => _, *}
  export dfhdl.core.{asValAny, asVarAny, asVarOf, asDclAny, asConstAny, cloneAnonValueAndDepsHere}
  extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized, P]]]
    def asUninitialized: DFVal[T, Modifier[A, C, Modifier.Initializable, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initializable, P]]]
end MetaDesign
