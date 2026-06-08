package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
//format: off
/** This stage transforms:
  *   - wait statements with time durations into cycles. For example (under 50Mhz clock):
  *     1.sec.wait -> 50000000.cy.wait
  *     2.ms.wait -> 100000.cy.wait
  */
//format: on
case object DropTimedRTWaits extends HierarchyStage:
  // DropForkJoinsRT / DropLocalBlocksRT must run before any RT-wait lowering so that RT fork-joins
  // are already lowered to the per-branch handshake form (and their local blocks flattened) before
  // waits are turned into FSM steps. Anchoring at the root of the RT-wait chain guarantees this.
  def dependencies: List[Stage] = List(DropLocalBlocksRT)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patches = subDB.members.collect {
      // replace wait statements with time durations to cycles
      case waitMember @ Wait(
            DFRef(duration @ DFTime.Val(_)),
            _,
            _,
            _
          ) if waitMember.isInRTDomain =>
        val dsn = new MetaDesign(
          waitMember,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT
        ):
          val waitTime = duration.getConstData[TimeNumber].toOption.get
          val clkRate: RateNumber = getSet.designDB.resolvedClkRstMap
            .get(waitMember.getOwnerDomain)
            .flatMap(_._1)
            .flatMap(_.rate.toOption)
            .get
          val cycles = (waitTime / clkRate.to_ps).value.toLong
          cycles.cy.wait(using dfc.setMeta(waitMember.meta))
        dsn.patch
    }.toList
    subDB.patch(patches)
  end transformSubDB
end DropTimedRTWaits

extension [T: HasDB](t: T)
  def dropTimedRTWaits(using CompilerOptions): DB =
    StageRunner.run(DropTimedRTWaits)(t.db)
