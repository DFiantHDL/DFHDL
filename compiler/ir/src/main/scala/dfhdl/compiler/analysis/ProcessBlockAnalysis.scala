package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import ir.ProcessBlock.Sensitivity
import scala.annotation.tailrec

extension (pb: ProcessBlock)
  def isInitial: Boolean =
    pb.sensitivity match
      case Sensitivity.Initial => true
      case _                   => false

extension (domainOwner: DFDomainOwner)(using MemberGetSet)
  // The resolved `@timing.reset` for this domain: walk the `@timing.related` chain (written by
  // `ExplicitClkRstCfg`) to the timing owner and read its resolved `@timing.reset` annotation.
  // A related link with `includeReset = false` severs the reset: the domain has no reset and
  // its registers/memories rely on their initial values instead of a reset-initialization
  // block. Note the clock relation is unaffected (resolved by a separate full-chain walk).
  def resolvedRstAnnot: Option[constraints.Timing.Reset] =
    @tailrec def resolve(owner: DFDomainOwner): Option[constraints.Timing.Reset] =
      owner.meta.annotations.collectFirst {
        case rel: constraints.Timing.Related => rel
      } match
        case Some(rel) if !rel.includeReset => None
        case Some(rel)                      => resolve(rel.ref.get)
        case None                           =>
          owner.meta.annotations.collectFirst { case rst: constraints.Timing.Reset => rst }
    resolve(domainOwner)
  end resolvedRstAnnot
end extension

extension (pb: ProcessBlock)(using MemberGetSet)
  def isSequential: Boolean =
    pb.sensitivity match
      case Sensitivity.All        => false
      case Sensitivity.Initial    => false
      case Sensitivity.List(refs) => true // TODO: fix this
  // The resolved reset presence of the block's domain. Used by the initial-block lowering
  // stages to decide between the `ToED` reset-branch path and declaration-init forms.
  def hasResolvedRstCfg: Boolean =
    pb.getOwnerDomain.resolvedRstAnnot.isDefined
end extension

// The declarations assigned by the given block members, ordered by first assignment.
// Used by the initial-block lowering stages to group/convert per-declaration content.
def assignedDcls(blockMembers: List[DFMember])(using MemberGetSet): List[DFVal.Dcl] =
  val assigned = scala.collection.mutable.LinkedHashSet.empty[DFVal.Dcl]
  blockMembers.foreach {
    case DFNet.BAssignment(toVal, _) =>
      toVal.departialDcl.foreach { (dcl, _) => assigned += dcl }
    case _ =>
  }
  assigned.toList

extension (member: DFMember)(using MemberGetSet)
  def isInInitialBlock: Boolean = member.isOwnedCond(cond = {
    case pb: ProcessBlock => Some(pb.isInitial)
    case _: DFDomainOwner => Some(false)
    case _                => None
  })

/** True when the given members (an RT process prologue or a first-step `onEntry` body, given as the
  * region's full flattened content) can be lowered into a generated `initial` block: every member
  * is one of
  *   - a blocking assignment with a constant RHS targeting a REG declaration (the `.din` form),
  *   - a combinational (`COMB_LOOP`) for loop with constant range bounds,
  *   - a conditional with constant guards/selectors,
  *   - an anonymous value dependency
  * (owner contents are vetted individually, as they are part of the flattened region). Text output
  * is NOT convertible (RT `initial` blocks reject it), so a printing prologue keeps the bootstrap
  * step. Vacuously true for an empty list (nothing to convert). Used by `DropRTWaits` to decide
  * whether the synthetic bootstrap step (Rule 6) is needed, and by `DropRTProcess` to gate the
  * initial-block generation.
  */
def isInitialConvertible(members: List[DFMember])(using MemberGetSet): Boolean =
  members.forall {
    case DFNet.BAssignment(toVal, fromVal) =>
      fromVal.isConst && toVal.departialDcl.exists((dcl, _) => dcl.isReg)
    // process-local declarations (e.g. SimplifyRTOps' iterator REG) and range bookkeeping
    // are neutral: only the statement closures are lowered into the initial block;
    // the rest stays in place (dcls are hoisted by DropLocalDcls, anons serve their users)
    case _: DFVal.Dcl          => true
    case _: DFRange            => true
    case fb: DFLoop.DFForBlock =>
      fb.isCombinational && {
        val range = fb.rangeRef.get
        Iterator(range.startRef, range.endRef, range.stepRef).forall(_.get.isConst)
      }
    case mh: DFConditional.DFMatchHeader => mh.selectorRef.get.isConst
    case cb: DFConditional.Block         =>
      cb.guardRef.get match
        case guard: DFVal => guard.isConst
        case _            => true // no guard (else branch / catch-all case)
    case dfVal: DFVal if dfVal.isAnonymous => true
    case _                                 => false
  }

/** The subset of the given RT process prologue (flattened, in order) that lowers into a generated
  * `initial` block: the effectful statements (assignment nets, combinational for loops with their
  * iterator/range bookkeeping, and constant-guarded conditional chains, each with their full
  * contents) along with their in-prologue anonymous value dependencies. Declarations, leftover
  * range bookkeeping, and anonymous values consumed by the process steps stay in place. Used by
  * `FlattenStepBlocks` (the forever wrap-around rotation clone) and by `DropRTProcess` (the
  * initial-block generation and the originals' removal), which must agree on the exact member set.
  */
def initialConvertibleMoveList(prologue: List[DFMember])(using MemberGetSet): List[DFMember] =
  val closure = scala.collection.mutable.Set.empty[DFMember]
  // anonymous value dependency closure (named/global values stay in place and remain
  // referenced from the moved clones)
  def addValDeps(dfVal: DFVal): Unit = closure ++= dfVal.collectRelMembers(false)
  def addStatement(m: DFMember): Unit = m match
    case net: DFNet            => closure ++= net :: net.collectRelMembers
    case fb: DFLoop.DFForBlock =>
      val range = fb.rangeRef.get
      closure += fb
      closure += fb.iteratorRef.get
      closure += range
      Iterator(range.startRef, range.endRef, range.stepRef).foreach(ref => addValDeps(ref.get))
      fb.members(MemberView.Folded).foreach(addStatement)
    case cb: DFConditional.Block =>
      closure += cb
      // the chain header (with its selector dependencies) plus the guard and pattern value
      // dependencies; a previous chain block is added on its own iteration turn
      cb.getRefs.foreach { ref =>
        ref.get match
          case mh: DFConditional.DFMatchHeader =>
            closure += mh
            addValDeps(mh.selectorRef.get)
          case header: DFConditional.Header => closure += header
          case dfVal: DFVal                 => addValDeps(dfVal)
          case _                            =>
      }
      cb.members(MemberView.Folded).foreach(addStatement)
    case _ => // dcls, ranges, and anons serving step members stay in place
  end addStatement
  prologue.foreach {
    // already collected as part of an enclosing owner
    case m if closure.contains(m) => ()
    case m                        => addStatement(m)
  }
  prologue.filter(closure)
end initialConvertibleMoveList
