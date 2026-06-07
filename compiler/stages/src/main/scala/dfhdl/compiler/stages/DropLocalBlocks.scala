package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec

//format: off
/** Flattens `LocalBlock`s into their enclosing owner. VHDL has no in-process named statement
  * block, so every local block (produced by `locally:`) is dissolved: its members are promoted
  * to the parent owner and the block itself is removed.
  *
  * This stage runs only for VHDL backends. For Verilog (all dialects) local blocks are emitted
  * natively as `begin : name ... end` named blocks and are left untouched.
  *
  * Fork-join branch local blocks are consumed earlier by [[DropForkJoins]]; by the time this
  * stage runs only standalone local blocks remain.
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
case object DropLocalBlocks extends HierarchyStage:
  def dependencies: List[Stage] = List(DropForkJoins, DropLocalDcls)
  def nullifies: Set[Stage] = Set()

  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case _: dfhdl.backends.vhdl => true
      case _                      => false

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
        case lb: LocalBlock if isInnermost(lb) =>
          // redirect all refs to `lb` (its children's ownerRefs) to `lb`'s owner, then remove `lb`.
          // the children keep their positions in the flat member list, now owned by the parent.
          lb -> Patch.Replace(lb.getOwner, Patch.Replace.Config.ChangeRefAndRemove)
      }.toList
    if (patches.isEmpty) db
    else dissolveRepeatedly(db.patch(patches))

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    dissolveRepeatedly(subDB)
end DropLocalBlocks

extension [T: HasDB](t: T)
  def dropLocalBlocks(using CompilerOptions): DB =
    StageRunner.run(DropLocalBlocks)(t.db)
