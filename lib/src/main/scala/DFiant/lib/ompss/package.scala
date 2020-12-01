package DFiant
package lib

import DFiant.DFDesign.DB.Patch
import DFiant.sim.DFSimDesign.Mode
import compiler.{IRCompilation, PreCompiler}
import constraints.timing.sync._

/**
  * This package enables basic integration with [[https://pm.bsc.es/ompss OmpSs]]
  * in the context of the [[https://legato-project.eu/ LEGaTO EU Horizon 2020 project]].
  */
package object ompss {
  /**
    * Sets a default vhdl93 compiler when using ompss
    */
  implicit val __defaultVHDL93 = compiler.backend.vhdl.v93

  /**
    * Sets a default precompiler that applies the required constraints on clock and reset when using ompss,
    * and adds a "_moved" suffix that is required for proper file/entity name integration.
    */
  implicit def __clkrstConstraints[D <: DFDesign] : PreCompiler[D] =
    (fromStage : IRCompilation[D]) => {
      //applying default ompss clock and reset constraints
      val constrained = fromStage tag new Tags {
        dsn !!! ClockParams("ap_clk", ClockParams.Edge.Rising)
        dsn !!! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
      }
      val top = constrained.db.top
      //the top level design should get a "_moved" suffix.
      //if we are in simulation, we don't rename the top.
      top.simMode match {
        case Mode.Off =>
          val newTop = top.copy(designType = s"${top.designType}_moved")
          constrained.newStage(constrained.db.patch(top -> Patch.Replace(newTop, Patch.Replace.Config.FullReplacement)))
        case Mode.On => constrained
      }
    }
}
