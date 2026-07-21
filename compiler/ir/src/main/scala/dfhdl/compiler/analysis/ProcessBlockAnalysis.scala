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

extension (pb: ProcessBlock)(using MemberGetSet)
  def isSequential: Boolean =
    pb.sensitivity match
      case Sensitivity.All        => false
      case Sensitivity.Initial    => false
      case Sensitivity.List(refs) => true // TODO: fix this
  // The resolved reset presence of the block's domain: walk the `@timing.related` chain to
  // the timing owner and look for the resolved `@timing.reset` annotation (written by
  // `ExplicitClkRstCfg`). Used by the initial-block lowering stages to decide between the
  // `ToED` reset-branch path and declaration-init forms.
  def hasResolvedRstCfg: Boolean =
    @tailrec def resolveTimingOwner(owner: DFDomainOwner): DFDomainOwner =
      val relatedTarget = owner.meta.annotations.collectFirst {
        case rel: constraints.Timing.Related => rel.ref.get
      }
      relatedTarget match
        case Some(target) => resolveTimingOwner(target)
        case None         => owner
    resolveTimingOwner(pb.getOwnerDomain).meta.annotations.exists {
      case _: constraints.Timing.Reset => true
      case _                           => false
    }
  end hasResolvedRstCfg
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

/** True when the given members (an RT process prologue or a first-step `onEntry` body) can be
  * lowered into a generated `initial` block: every member is either an anonymous value dependency
  * or a blocking assignment with a constant RHS targeting a REG declaration (the `.din` form).
  * Vacuously true for an empty list (nothing to convert). Used by `DropRTWaits` to decide whether
  * the synthetic bootstrap step (Rule 6) is needed, and by `DropRTProcess` to gate the
  * initial-block generation.
  */
def isInitialConvertible(members: List[DFMember])(using MemberGetSet): Boolean =
  members.forall {
    case DFNet.BAssignment(toVal, fromVal) =>
      fromVal.isConst && toVal.departialDcl.exists((dcl, _) => dcl.isReg)
    // process-local declarations (e.g. SimplifyRTOps' iterator REG) and range bookkeeping
    // are neutral — only the assignment-net closures are lowered into the initial block;
    // the rest stays in place (dcls are hoisted by DropLocalDcls, anons serve their users)
    case _: DFVal.Dcl                      => true
    case _: DFRange                        => true
    case dfVal: DFVal if dfVal.isAnonymous => true
    case _                                 => false
  }
