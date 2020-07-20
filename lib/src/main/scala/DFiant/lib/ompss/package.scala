package DFiant
package lib

import compiler.{IRCompilation, PreCompiler}
import constraints.timing.sync._

package object ompss {
  implicit def defaultDoesNothing[D <: DFDesign] : PreCompiler[D] =
    (fromStage : IRCompilation[D]) => fromStage !! new Tags {
      dsn !! ClockParams("ap_clk", ClockParams.Edge.Rising)
      dsn !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
    }
}
