package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DomainType
import dfhdl.core.{assign, nbassign}
import scala.annotation.tailrec

// frontend Bit value alias (kept fully-qualified to avoid clashing with `ir.DFBit`)
private type BitFE = dfhdl.core.DFValOf[dfhdl.core.DFBit]

//format: off
/** Lowers fork-join into multiple concurrent processes synchronized by start/done handshake
  * signals.
  *
  * Only `forkJoin` (join-all) is ever lowered: the parent blocks until *all* branches complete, so
  * a fixed set of synthesized processes faithfully represents it (a looping parent never overlaps
  * with still-running branches). `forkJoinAny` / `forkJoinNone` let the parent continue while
  * branches keep running, which can dynamically spawn overlapping threads (e.g. a fork inside a
  * loop) that a static lowering cannot represent — so they are *never* lowered. They are emitted
  * natively only where supported (SystemVerilog `join_any` / `join_none`) and rejected by the
  * backend elsewhere; under register-transfer (RT) they are rejected at the frontend.
  *
  * The lowering structure is domain-agnostic and shared by two concrete stages:
  *   - [[DropForkJoinsED]] lowers join-all fork-joins in the event-driven (ED) domain for VHDL,
  *     which has no native fork-join. Old Verilog (v95/v2001) and SystemVerilog emit `fork ... join`
  *     natively, so nothing is lowered there. Handshake signals are plain `Bit <> VAR` driven with
  *     non-blocking assignments.
  *   - [[DropForkJoinsRT]] lowers join-all fork-joins in the register-transfer (RT) domain into the
  *     per-branch `process` + handshake form, which the RT->FSM pipeline (`SimplifyRTOps`
  *     -> `DropRTWaits` -> `DropRTProcess`) then turns into a clocked state-machine. Handshake
  *     signals must hold value across clock edges, so they are registered (`Bit <> VAR.REG`) and
  *     driven with blocking assignments (equivalent to `.din :=`).
  *
  * For a fork with branch local-blocks `b0..bN` the lowering:
  *   1. allocates a `<fork>_start_i` / `<fork>_done_i` Bit signal pair per branch in the enclosing
  *      domain, declared right *before* the parent process;
  *   2. replaces the fork in the parent process with: drive every `start_i` high, a join wait
  *      (All: wait for all dones; Any: wait for any done; None: no wait), then re-arm the `start_i`
  *      signals (All/Any only);
  *   3. emits one `process` per branch (placed right *after* the parent process) that waits
  *      for its `start_i`, runs the branch body, raises `done_i`, then waits for `start_i` to drop
  *      and clears `done_i`.
  *
  * The branch body is kept inside its `LocalBlock`; for VHDL [[DropLocalBlocksED]] later flattens
  * it (Verilog keeps it as a native `begin ... end` block), and for RT [[DropLocalBlocksRT]]
  * flattens it before the FSM pipeline (RT process bodies cannot carry local blocks).
  *
  * Notes / limitations:
  *   - Multiple branches assigning the same signal is the user's responsibility, exactly as in
  *     SystemVerilog (this stage does not arbitrate writes).
  *   - `join_none` branches are single-shot: the parent does not re-arm the handshake.
  *   - Nested forks are lowered innermost-first across repeated passes so an outer and inner fork
  *     are never patched together.
  */
//format: on
private abstract class DropForkJoins extends HierarchyStage:
  def nullifies: Set[Stage] = Set()

  // which forks this stage is responsible for (selected by domain)
  protected def handlesFork(fork: ForkBlock)(using MemberGetSet): Boolean
  // whether a handled fork needs lowering for the current target
  protected def needsLowering(fork: ForkBlock)(using CompilerOptions): Boolean
  // the domain of the generated members (drives the MetaDesign domainType)
  protected def metaDesignDomain: DomainType
  // declare the per-branch start/done handshake signal pair (ED: `Bit <> VAR`, RT: `Bit <> VAR.REG`).
  // returns the signals patch and the start/done signal lists (referenced by the parent/branch
  // designs below). The signal modifier is the only emit detail that needs a statically-known
  // domain, so each concrete stage builds this design itself.
  protected def makeSignals(parentProc: ProcessBlock, forkName: String, count: Int)(using
      MemberGetSet,
      RefGen
  ): (DFMember, Patch, List[BitFE], List[BitFE])
  // drive a handshake bit. ED uses a non-blocking assignment; RT uses a (registered) blocking
  // assignment — equivalent to `.din :=` on the reg signal. Both are constraint-free at the IR
  // level, so the parent/branch designs below are shared across domains.
  protected def driveBit(sig: BitFE, value: BitFE)(using dfhdl.core.DFC): Unit

  // a constraint-free Bit literal for the handshake drives
  protected final def bitConst(b: Boolean)(using dfhdl.core.DFC): BitFE =
    dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(b))

  // A fork that contains no nested fork — safe to lower this pass. Nested forks are lowered
  // innermost-first across repeated passes so an outer and inner fork are never patched together.
  private def isInnermost(fork: ForkBlock)(using MemberGetSet): Boolean =
    !fork.members(MemberView.Flattened).exists {
      case _: ForkBlock => true
      case _            => false
    }

  // removal patches for every relocated member (branch local blocks + their descendants)
  protected final def branchRemovalPatches(
      branches: List[LocalBlock]
  )(using MemberGetSet): List[(DFMember, Patch)] =
    branches
      .flatMap(branchLB => branchLB :: branchLB.members(MemberView.Flattened))
      .map(m => m -> Patch.Remove(isMoved = true))

  @tailrec private def lowerRepeatedly(db: DB)(using RefGen, CompilerOptions): DB =
    given MemberGetSet = db.getSet
    val patches = db.members.view.collect {
      case fork: ForkBlock if handlesFork(fork) && isInnermost(fork) && needsLowering(fork) => fork
    }.flatMap(lowerFork).toList
    if (patches.isEmpty) db
    else lowerRepeatedly(db.patch(patches))

  protected final def lowerFork(fork: ForkBlock)(using
      MemberGetSet,
      RefGen
  ): List[(DFMember, Patch)] =
    val branches = fork.members(MemberView.Folded).collect { case lb: LocalBlock => lb }
    // degenerate empty fork: just drop it
    if (branches.isEmpty) List(fork -> Patch.Remove(isMoved = false))
    else
      val parentProc = fork.getOwnerProcessBlock
      val forkName = fork.getName
      val joinMode = fork.join

      // (1): handshake signals, declared right *before* the parent process.
      val (sigMember, sigPatch, startSignals, doneSignals) =
        makeSignals(parentProc, forkName, branches.length)

      // (3): one forever-process per branch, placed right *after* the parent process.
      val branchesDsn =
        new MetaDesign(parentProc, Patch.Add.Config.After, domainType = metaDesignDomain):
          branches.lazyZip(startSignals).lazyZip(doneSignals).foreach { (branchLB, start, done) =>
            process.forever {
              waitUntil(start)
              // relocate the branch local block (and its whole subtree) into this process
              plantMembers(fork, branchLB :: branchLB.members(MemberView.Flattened))
              driveBit(done, bitConst(true))
              waitWhile(start)
              driveBit(done, bitConst(false))
            }
          }

      // (2): replace the fork in the parent process with the trigger + join-wait logic.
      val parentDsn =
        new MetaDesign(
          fork,
          Patch.Add.Config.ReplaceWithLast(
            Patch.Replace.Config.FullReplacement,
            // the fork's only references are its branch local-blocks' ownerRefs (from inside the
            // fork); those are handled by the relocation above, so we must not redirect them onto
            // a generated statement here.
            Patch.Replace.RefFilter.Outside(fork)
          ),
          domainType = metaDesignDomain
        ):
          // the replacement lands inside the parent process, so assert process scope to permit
          // waits directly (no nested process).
          given dfhdl.core.DFC.Scope.Process = dfhdl.core.DFC.Scope.Process
          startSignals.foreach(s => driveBit(s, bitConst(true)))
          joinMode match
            case ForkBlock.Join.All =>
              waitUntil(doneSignals.reduce(_ && _))
              startSignals.foreach(s => driveBit(s, bitConst(false)))
            case ForkBlock.Join.Any =>
              waitUntil(doneSignals.reduce(_ || _))
              startSignals.foreach(s => driveBit(s, bitConst(false)))
            case ForkBlock.Join.None =>
            // no wait, no re-arm (single-shot; see limitations)

      List((sigMember, sigPatch), branchesDsn.patch) ++
        branchRemovalPatches(branches) ++ List(parentDsn.patch)
    end if
  end lowerFork

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    lowerRepeatedly(subDB)
end DropForkJoins

// Lowers ED-domain join-all fork-joins. Runs after RT->ED conversion; gated to VHDL, the only
// backend with no native fork-join. Old Verilog and SystemVerilog emit `fork ... join` natively,
// and `join_any` / `join_none` are never lowered (see the class doc).
case object DropForkJoinsED extends DropForkJoins:
  def dependencies: List[Stage] = List(ToED)

  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case _: dfhdl.backends.vhdl => true
      case _                      => false

  protected def metaDesignDomain: DomainType = DomainType.ED

  protected def handlesFork(fork: ForkBlock)(using MemberGetSet): Boolean = fork.isInEDDomain

  // Only join-all is lowered (VHDL has no native fork-join). join-any / join-none are never
  // lowered — they stay as fork blocks and the VHDL printer rejects them (unsupported).
  protected def needsLowering(fork: ForkBlock)(using co: CompilerOptions): Boolean =
    fork.join == ForkBlock.Join.All

  protected def makeSignals(parentProc: ProcessBlock, forkName: String, count: Int)(using
      MemberGetSet,
      RefGen
  ): (DFMember, Patch, List[BitFE], List[BitFE]) =
    val dsn =
      new MetaDesign(parentProc, Patch.Add.Config.Before, domainType = DomainType.ED):
        val startSignals = (0 until count).toList.map { i =>
          (Bit <> VAR)(using dfc.setName(s"${forkName}_start_$i"))
        }
        val doneSignals = (0 until count).toList.map { i =>
          (Bit <> VAR)(using dfc.setName(s"${forkName}_done_$i"))
        }
    val (m, p) = dsn.patch
    (m, p, dsn.startSignals, dsn.doneSignals)

  protected def driveBit(sig: BitFE, value: BitFE)(using dfhdl.core.DFC): Unit =
    sig.nbassign(value)
end DropForkJoinsED

// Lowers RT-domain `forkJoin` (join-all) into the per-branch handshake form, *before* the RT->FSM
// pipeline. Always runs (RT fork-join has no native HDL target — it must always become a clocked
// FSM, regardless of backend). `forkJoinAny` / `forkJoinNone` are rejected at the frontend under
// RT, so only join-all reaches here. Handshake signals are registered so they hold across edges.
case object DropForkJoinsRT extends DropForkJoins:
  // No dependencies: this stage runs at the very front of the RT lowering, on the raw RT-domain
  // forks, before the RT-wait pipeline. It deliberately does NOT depend on ToRT — RT forks are
  // written directly in RT-domain processes (already RT after elaboration), and depending on ToRT
  // would drag the whole DF->RT conversion chain into the RT-wait pipeline's dependency closure,
  // changing the behaviour of stages (e.g. SimplifyRTOps) tested in isolation.
  def dependencies: List[Stage] = List()

  override def runCondition(using co: CompilerOptions): Boolean = true

  protected def metaDesignDomain: DomainType = DomainType.RT

  protected def handlesFork(fork: ForkBlock)(using MemberGetSet): Boolean = fork.isInRTDomain

  // only join-all is supported under RT (join-any / join-none are rejected at the frontend)
  protected def needsLowering(fork: ForkBlock)(using co: CompilerOptions): Boolean =
    fork.join == ForkBlock.Join.All

  protected def makeSignals(parentProc: ProcessBlock, forkName: String, count: Int)(using
      MemberGetSet,
      RefGen
  ): (DFMember, Patch, List[BitFE], List[BitFE]) =
    val dsn =
      new MetaDesign(parentProc, Patch.Add.Config.Before, domainType = DomainType.RT):
        val startSignals = (0 until count).toList.map { i =>
          (Bit <> VAR.REG)(using dfc.setName(s"${forkName}_start_$i"))
        }
        val doneSignals = (0 until count).toList.map { i =>
          (Bit <> VAR.REG)(using dfc.setName(s"${forkName}_done_$i"))
        }
    val (m, p) = dsn.patch
    (m, p, dsn.startSignals, dsn.doneSignals)

  // registered blocking assignment (equivalent to `.din :=` on the reg signal)
  protected def driveBit(sig: BitFE, value: BitFE)(using dfhdl.core.DFC): Unit =
    sig.assign(value)
end DropForkJoinsRT

extension [T: HasDB](t: T)
  def dropForkJoinsED(using CompilerOptions): DB =
    StageRunner.run(DropForkJoinsED)(t.db)
  def dropForkJoinsRT(using CompilerOptions): DB =
    StageRunner.run(DropForkJoinsRT)(t.db)
