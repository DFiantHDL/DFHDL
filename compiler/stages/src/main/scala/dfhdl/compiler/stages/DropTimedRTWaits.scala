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
case object DropTimedRTWaits extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.members.collect {
      // replace wait statements with time durations to cycles
      case waitMember @ Wait(
            DFRef(duration @ DFPhysical.Val(DFPhysical(DFPhysical.Unit.Time))),
            _,
            _,
            _
          ) if waitMember.isInRTDomain =>
        val dsn = new MetaDesign(
          waitMember,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
        ):
          val (waitValue: BigDecimal, waitUnit: DFPhysical.Unit.Time.Scale) =
            duration.getConstData.get: @unchecked
          val (RTDomainCfg.Explicit(clkCfg = ClkCfg.Explicit(rate = clkRate))) =
            designDB.explicitRTDomainCfgMap(waitMember.getOwnerDomain): @unchecked
          val (clkRateValue: BigDecimal, clkRateUnitScale) =
            clkRate.getConstData.get: @unchecked
          val clkRatePs = (clkRateUnitScale: @unchecked) match
            case freq: DFPhysical.Unit.Freq.Scale   => freq.to_ps(clkRateValue)
            case period: DFPhysical.Unit.Time.Scale => period.to_ps(clkRateValue)
          val waitTime = waitUnit.to_ps(waitValue)
          val cycles = (waitTime / clkRatePs).toLong
          cycles.cy.wait(using dfc.setMeta(waitMember.meta))
        dsn.patch
    }

    designDB.patch(patchList)
  end transform
end DropTimedRTWaits

extension [T: HasDB](t: T)
  def dropTimedRTWaits(using CompilerOptions): DB =
    StageRunner.run(DropTimedRTWaits)(t.db)
