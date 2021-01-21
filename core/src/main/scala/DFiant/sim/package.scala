package DFiant

import sim.DFSimMember.{Assert, Finish}
import Assert.{Message}
import internals._

package object sim {
  ////////////////////////////////////////////////////////////////////////////////////
  // Simulation-related constructs
  ////////////////////////////////////////////////////////////////////////////////////
  def inSimulation(implicit ctx: DFAny.Context): Boolean =
    ctx.db.top.simMode match {
      case DFSimDesign.Mode.Off => false
      case DFSimDesign.Mode.On  => true
    }

  def assert[C, M](cond : Exact[C], msg : M, severity : Severity = Warning)(
    implicit ctx : DFAny.Context, m : M => Message, condArg : DFBool.Arg[C]
  ) : FSM.Capable = {
    if (inSimulation) Assert(Some(condArg(cond)), m(msg), severity) else new FSM.Capable {}
  }
  def report[M](msg : M, severity : Severity = Note)(implicit ctx : DFAny.Context, m : M => Message) : FSM.Capable = {
    if (inSimulation) Assert(None, m(msg), severity) else new FSM.Capable {}
  }
  def finish()(implicit ctx: DFAny.Context): FSM.Capable = {
    if (inSimulation) Finish() else new FSM.Capable {}
  }
  ////////////////////////////////////////////////////////////////////////////////////

}
