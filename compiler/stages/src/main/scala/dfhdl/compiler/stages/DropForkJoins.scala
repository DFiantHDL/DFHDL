package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DomainType.ED
import dfhdl.compiler.stages.verilog.VerilogDialect
import scala.annotation.tailrec

//format: off
/** Lowers fork-join (`forkJoin` / `forkJoinAny` / `forkJoinNone`) into multiple concurrent
  * processes synchronized by start/done handshake signals.
  *
  * SystemVerilog (sv2005+) emits `fork ... join[_any|_none]` natively. VHDL has no fork-join at
  * all, so every mode is lowered. Old Verilog (v95/v2001) supports `fork ... join` (wait-all)
  * natively and only lacks `join_any` / `join_none` (SystemVerilog 2005+), so under those dialects
  * only `forkJoinAny` / `forkJoinNone` are lowered while `forkJoin` is left native.
  *
  * For a fork with branch local-blocks `b0..bN` the stage:
  *   1. allocates a `<fork>_start_i` / `<fork>_done_i` Bit signal pair per branch in the
  *      enclosing domain;
  *   2. replaces the fork in the parent process with: drive every `start_i` high, a join wait
  *      (All: wait for all dones; Any: wait for any done; None: no wait), then re-arm the
  *      `start_i` signals (All/Any only);
  *   3. emits one `process` per branch (a forever loop) that waits for its `start_i`, runs the
  *      branch body, raises `done_i`, then waits for `start_i` to drop and clears `done_i`.
  *
  * The branch body is kept inside its `LocalBlock`; for VHDL [[DropLocalBlocks]] later flattens
  * it, while Verilog keeps it as a native `begin ... end` block.
  *
  * Notes / limitations:
  *   - Multiple branches assigning the same signal is the user's responsibility, exactly as in
  *     SystemVerilog (this stage does not arbitrate writes).
  *   - `join_none` branches are single-shot: the parent does not re-arm the handshake, so a
  *     parent that re-enters the fork before a `join_none` branch finishes is a race (mirrors the
  *     inherent difficulty of mapping dynamic process spawning onto static RTL processes).
  *   - The handshake uses `waitUntil`/`waitWhile`, which lower to correct `wait until ...` (VHDL)
  *     and `wait(...)` (Verilog) — verified equivalent to native SystemVerilog fork-join by
  *     simulation.
  */
//format: on
case object DropForkJoins extends HierarchyStage:
  def dependencies: List[Stage] = List(ToED)
  def nullifies: Set[Stage] = Set()

  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case _: dfhdl.backends.vhdl     => true
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false

  // A fork that contains no nested fork — safe to lower this pass. Nested forks are lowered
  // innermost-first across repeated passes (Pattern 9) so an outer and inner fork are never
  // patched together.
  private def isInnermost(fork: ForkBlock)(using MemberGetSet): Boolean =
    !fork.members(MemberView.Flattened).exists {
      case _: ForkBlock => true
      case _            => false
    }

  // Whether this fork needs lowering for the target backend. VHDL has no fork-join at all, so
  // every mode is lowered. Old Verilog (v95/v2001) supports `fork ... join` (wait-all) natively
  // and only lacks `join_any` / `join_none` (SystemVerilog 2005+), so only those are lowered.
  private def needsLowering(fork: ForkBlock)(using co: CompilerOptions): Boolean =
    co.backend match
      case _: dfhdl.backends.vhdl     => true
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 =>
            fork.join != ForkBlock.Join.All
          case _ => false // sv2005+: emitted natively, never lowered
      case _ => false

  @tailrec private def lowerRepeatedly(db: DB)(using RefGen, CompilerOptions): DB =
    given MemberGetSet = db.getSet
    val patches = db.members.view.collect {
      case fork: ForkBlock if isInnermost(fork) && needsLowering(fork) => fork
    }.flatMap(lowerFork).toList
    if (patches.isEmpty) db
    else lowerRepeatedly(db.patch(patches))

  private def lowerFork(fork: ForkBlock)(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
    val branches = fork.members(MemberView.Folded).collect { case lb: LocalBlock => lb }
    // degenerate empty fork: just drop it
    if (branches.isEmpty) List(fork -> Patch.Remove(isMoved = false))
    else
      val parentProc = fork.getOwnerProcessBlock
      val forkName = fork.getName
      val joinMode = fork.join

      // (1): handshake signals, declared right *before* the parent process. Both the parent
      // (trigger + join-wait) and every branch process reference these, so declaring them
      // first keeps all references pointing backward (valid member order without relying on a
      // later ordering stage).
      val signalsDsn =
        new MetaDesign(parentProc, Patch.Add.Config.Before, domainType = ED):
          val startSignals = branches.indices.toList.map { i =>
            (Bit <> VAR)(using dfc.setName(s"${forkName}_start_$i"))
          }
          val doneSignals = branches.indices.toList.map { i =>
            (Bit <> VAR)(using dfc.setName(s"${forkName}_done_$i"))
          }

      // (3): one forever-process per branch, placed right *after* the parent process.
      val branchesDsn =
        new MetaDesign(parentProc, Patch.Add.Config.After, domainType = ED):
          branches.lazyZip(signalsDsn.startSignals).lazyZip(signalsDsn.doneSignals).foreach {
            (branchLB, start, done) =>
              process.forever {
                waitUntil(start)
                // relocate the branch local block (and its whole subtree) into this process
                plantMembers(fork, branchLB :: branchLB.members(MemberView.Flattened))
                done :== 1
                waitWhile(start)
                done :== 0
              }
          }

      // removal patches for every relocated member (branch local blocks + their descendants)
      val relocatedMembers =
        branches.flatMap(branchLB => branchLB :: branchLB.members(MemberView.Flattened))
      val removalPatches = relocatedMembers.map(m => m -> Patch.Remove(isMoved = true))

      // (2): replace the fork in the parent process with the trigger + join-wait logic.
      val parentDsn =
        new MetaDesign(
          fork,
          Patch.Add.Config.ReplaceWithLast(
            Patch.Replace.Config.FullReplacement,
            // the fork's only references are its branch local-blocks' ownerRefs (from inside
            // the fork); those are handled by the relocation above, so we must not redirect
            // them onto a generated statement here.
            Patch.Replace.RefFilter.Outside(fork)
          ),
          domainType = ED
        ):
          // the replacement lands inside the parent process, so assert process scope to
          // permit non-blocking assignments and waits directly (no nested process).
          given dfhdl.core.DFC.Scope.Process = dfhdl.core.DFC.Scope.Process
          signalsDsn.startSignals.foreach(_ :== 1)
          joinMode match
            case ForkBlock.Join.All =>
              waitUntil(signalsDsn.doneSignals.reduce(_ && _))
              signalsDsn.startSignals.foreach(_ :== 0)
            case ForkBlock.Join.Any =>
              waitUntil(signalsDsn.doneSignals.reduce(_ || _))
              signalsDsn.startSignals.foreach(_ :== 0)
            case ForkBlock.Join.None =>
            // no wait, no re-arm (single-shot; see limitations)

      List(signalsDsn.patch, branchesDsn.patch) ++ removalPatches ++ List(parentDsn.patch)
    end if
  end lowerFork

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    lowerRepeatedly(subDB)
end DropForkJoins

extension [T: HasDB](t: T)
  def dropForkJoins(using CompilerOptions): DB =
    StageRunner.run(DropForkJoins)(t.db)
