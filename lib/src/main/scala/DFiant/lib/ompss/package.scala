package DFiant
package lib

import compiler.{AddTags, IRCompilation, PreCompiler}
import shapeless.{:: => #:}
import constraints.timing.sync._

package object ompss {
  implicit def defaultDoesNothing[D <: DFDesign, H <: shapeless.HList] : PreCompiler[D, H, AddTags #: H] =
    (fromStage : IRCompilation[D, H]) => fromStage !! new Tags {
      dsn !! ClockParams("ap_clk", ClockParams.Edge.Rising)
      dsn !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
    }
}
