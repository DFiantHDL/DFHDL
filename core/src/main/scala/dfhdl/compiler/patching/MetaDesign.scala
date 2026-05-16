package dfhdl.compiler.patching
import dfhdl.core.*
import dfhdl.compiler.ir
import Patch.Add.Config as AddCfg
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.compiler.ir.DFDesignBlock

type MetaDesignAny = MetaDesign[DomainType]

/** A base class for synthesising new IR members inside a compiler stage and injecting them into the
  * DB at a precise position relative to an anchor member.
  *
  * Extend this class anonymously inside a stage's `transform` method to write ordinary DFHDL DSL
  * code (ports, vars, assignments, etc.). The members created in the body are extracted and
  * inserted into the DB when `dsn.patch` is applied; the MetaDesign block itself is discarded.
  *
  * ===Constructing raw IR members inside the body===
  *
  * To build an IR member directly (e.g. `ir.Goto`) rather than through the DSL, add
  * `import dfhdl.core.*` at the top of the anonymous body. This brings `refTW` and `addMember` into
  * scope. Use `dfc.ownerOrEmptyRef` to obtain the owner ref — do not use `dfc.owner.ref` as that
  * extension method may conflict with other imports:
  * {{{
  *   val dsn = new MetaDesign(anchor, Patch.Add.Config.Before):
  *     import dfhdl.core.*
  *     ir.Goto(existingStep.refTW[ir.Goto], dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags).addMember
  * }}}
  *
  * ===Ownership===
  *
  * For `Before` / `After` / `ReplaceWith*` configs, newly created members are placed as siblings of
  * the anchor (same owner). For `InsideFirst` / `InsideLast`, members are placed as direct children
  * of the anchor (which must be a `DFOwner`).
  */
abstract class MetaDesign[+D <: DomainType](
    positionMember: ir.DFMember,
    addCfg: AddCfg,
    domainType: D = DomainType.DF
)(using
    getSet: ir.MemberGetSet,
    refGen: ir.RefGen
) extends Design
    with reflect.Selectable:
  export dfhdl.hw.flag.{scalaPrints, scalaAsserts, scalaRanges}
  lazy val patch = positionMember -> Patch.Add(this, addCfg)
  private lazy val globalInjection: Boolean =
    (addCfg, positionMember) match
      case (_, dfVal: ir.DFVal.CanBeGlobal)                                     => dfVal.isGlobal
      case (AddCfg.Before, positionOwner: DFDesignBlock) if positionOwner.isTop => true
      case _                                                                    => false
  lazy val injectedOwner: ir.DFOwner = addCfg match
    case AddCfg.InsideFirst | AddCfg.InsideLast =>
      positionMember match
        case positionOwner: ir.DFOwner => positionOwner
        case _ => throw new IllegalArgumentException("Expecting owner member for AddInside config.")
    case _ if globalInjection => getSet.designDB.top
    case _                    => positionMember.getOwner
  final override private[dfhdl] def initOwner: Design.Block =
    dfc.mutableDB.addMember(injectedOwner)
    injectedOwner.getThisOrOwnerDesign.asFE
  // default context position is set according to the positionMember
  final override protected def __dfc: DFC =
    DFC.emptyNoEO.copy(refGen = refGen, position = positionMember.meta.position)

  injectedOwner match
    case design: ir.DFDesignBlock => // do nothing
    case _                        =>
      dfc.enterOwner(injectedOwner.asFE)

  dfc.mutableDB.injectMetaGetSet(getSet)
  final type TDomain = D @uncheckedVariance
  final protected given TDomain = domainType
  // we don't really care about the IR domain type of a meta design, since it is removed.
  // only the compile-time domain type is important for meta-programming with the relevant constraints.
  final lazy val __domainType: ir.DomainType = ir.DomainType.DF

  final def plantMember[T <: ir.DFMember](member: T): T =
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
  final def plantClonedMembers(
      baseOwner: ir.DFOwner,
      members: List[ir.DFMember]
  ): Map[ir.DFMember, ir.DFMember] =
    val clonedMemberMap = members.map { m => m -> m.copyWithNewRefs }.toMap
    members.foreach { m =>
      val cloned = clonedMemberMap(m)
      dfc.mutableDB.addMember(cloned)
      val owner = clonedMemberMap.getOrElse(m.getOwner, dfc.owner.asIR).asInstanceOf[ir.DFOwner]
      dfc.mutableDB.newRefFor(cloned.ownerRef, owner)
      m.getRefs.lazyZip(cloned.getRefs).foreach { (ref, clonedRef) =>
        val refMember = ref.get
        dfc.mutableDB.newRefFor(clonedRef, clonedMemberMap.getOrElse(refMember, refMember))
      }
    }
    clonedMemberMap
  end plantClonedMembers
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate erroneous designs
  final override private[dfhdl] def skipChecks: Boolean = true

  export dfhdl.hdl.{assert => _, *}
  export dfhdl.core.{asValAny, asVarAny, asVarOf, asDclAny, asConstAny, cloneAnonValueAndDepsHere}
  export dfhdl.core.IntParam.*
  extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized, P]]]
    def asUninitialized: DFVal[T, Modifier[A, C, Modifier.Initializable, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initializable, P]]]
end MetaDesign
