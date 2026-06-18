package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec

//format: off
/** Flattens `LocalBlock`s into their enclosing owner: each local block (produced by `locally:`) is
  * dissolved, promoting its members to the parent owner and removing the block itself.
  *
  * The dissolve logic is domain-agnostic and shared by two concrete stages:
  *   - [[DropLocalBlocksED]] runs only for VHDL backends, which have no in-process named statement
  *     block. For Verilog (all dialects) local blocks are emitted natively as `begin : name ... end`
  *     and are left untouched. Fork-join branch local blocks are consumed earlier by
  *     [[DropForkJoinsED]]; by the time this stage runs only standalone local blocks remain.
  *   - [[DropLocalBlocksRT]] runs always (any backend) and flattens *all* RT-domain local blocks —
  *     both standalone `locally:` blocks and fork-join branch wrappers (produced by
  *     [[DropForkJoinsRT]]) — *before* the RT->FSM pipeline. An RT process body cannot carry a
  *     `LocalBlock`: the RT->FSM step extraction (`DropRTWaits` / `FlattenStepBlocks` /
  *     `DropRTProcess`) only recurses through `StepBlock`s, so waits/statements nested in a local
  *     block would be invisible to it.
  *
  * ==Rule: dissolve local block==
  * {{{
  * // Before
  * process:
  *   x :== 1
  *   locally:
  *     y :== 2
  *     z :== 3
  *
  * // After
  * process:
  *   x :== 1
  *   y :== 2
  *   z :== 3
  * }}}
  *
  * Nested local blocks are dissolved innermost-first across repeated passes.
  */
//format: on
private abstract class DropLocalBlocks extends HierarchyStage:
  def nullifies: Set[Stage] = Set()

  // which local blocks this stage is responsible for (selected by domain)
  protected def handlesBlock(lb: LocalBlock)(using MemberGetSet): Boolean

  // A local block that does not itself contain another local block (safe to dissolve this pass).
  private def isInnermost(lb: LocalBlock)(using MemberGetSet): Boolean =
    !lb.members(MemberView.Flattened).exists {
      case _: LocalBlock => true
      case _             => false
    }

  @tailrec private def dissolveRepeatedly(db: DB): DB =
    given MemberGetSet = db.getSet
    val patches: List[(DFMember, Patch)] =
      db.members.view.collect {
        case lb: LocalBlock if handlesBlock(lb) && isInnermost(lb) =>
          // redirect all refs to `lb` (its children's ownerRefs) to `lb`'s owner, then remove `lb`.
          // the children keep their positions in the flat member list, now owned by the parent.
          lb -> Patch.Replace(lb.getOwner, Patch.Replace.Config.ChangeRefAndRemove)
      }.toList
    if (patches.isEmpty) db
    else dissolveRepeatedly(db.patch(patches))

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    dissolveRepeatedly(subDB)
end DropLocalBlocks

// Flattens ED-domain local blocks for VHDL (which has no named statement block). Verilog keeps
// them native, so this stage is a no-op there.
case object DropLocalBlocksED extends DropLocalBlocks:
  def dependencies: List[Stage] = List(DropForkJoinsED, DropLocalDcls)

  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case _: dfhdl.backends.vhdl => true
      case _                      => false

  protected def handlesBlock(lb: LocalBlock)(using MemberGetSet): Boolean = lb.isInEDDomain
end DropLocalBlocksED

// Flattens all RT-domain local blocks before the RT->FSM pipeline (RT process bodies cannot carry
// local blocks). Always runs, regardless of backend.
case object DropLocalBlocksRT extends DropLocalBlocks:
  // Only needs the forks lowered first (so branch local blocks exist as standalone blocks). Unlike
  // the ED variant it must NOT depend on DropLocalDcls: this stage runs early, before the RT-wait
  // pipeline, whereas DropLocalDcls belongs late in BackendPrepStage — pulling it in here would
  // force it to run far too early and corrupt the pipeline order.
  def dependencies: List[Stage] = List(DropForkJoinsRT)

  override def runCondition(using co: CompilerOptions): Boolean = true

  protected def handlesBlock(lb: LocalBlock)(using MemberGetSet): Boolean = lb.isInRTDomain
end DropLocalBlocksRT

extension [T: HasDB](t: T)
  def dropLocalBlocksED(using CompilerOptions): DB =
    StageRunner.run(DropLocalBlocksED)(t.db)
  def dropLocalBlocksRT(using CompilerOptions): DB =
    StageRunner.run(DropLocalBlocksRT)(t.db)
