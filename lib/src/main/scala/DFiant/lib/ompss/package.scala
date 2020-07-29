package DFiant
package lib

import DFiant.DFDesign.DB.Patch
import DFiant.sim.DFSimDesign.Mode
import compiler.{IRCompilation, PreCompiler}
import constraints.timing.sync._

package object ompss {
  implicit val __defaultVHDL93 = compiler.backend.vhdl.v93

  implicit def __clkrstConstraints[D <: DFDesign] : PreCompiler[D] =
    (fromStage : IRCompilation[D]) => {
      //applying default ompss clock and reset constraints
      val constrained = fromStage !! new Tags {
        dsn !! ClockParams("ap_clk", ClockParams.Edge.Rising)
        dsn !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
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
