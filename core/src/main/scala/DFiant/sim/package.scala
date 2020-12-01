package DFiant

import sim.DFSimMember.{Assert, Finish}
import Assert.{Message}
import internals._

package object sim {
  ////////////////////////////////////////////////////////////////////////////////////
  // Simulation-related constructs
  ////////////////////////////////////////////////////////////////////////////////////
  def inSimulation(implicit ctx : DFAny.Context) : Boolean = ctx.db.top.simMode match {
    case DFSimDesign.Mode.Off => false
    case DFSimDesign.Mode.On => true
  }

  def assert[C](cond : Exact[C], msg : Message, severity : Severity = Warning)(
    implicit ctx : DFAny.Context, condArg : DFBool.Arg[C]
  ) : FSM.Capable = {
    if (inSimulation) Assert(Some(condArg(cond)), msg, severity) else new FSM.Capable {}
  }
  def report(msg : Message, severity : Severity = Note)(implicit ctx : DFAny.Context) : FSM.Capable = {
    if (inSimulation) Assert(None, msg, severity) else new FSM.Capable {}
  }
  def finish()(implicit ctx : DFAny.Context) : FSM.Capable = {
    if (inSimulation) Finish() else new FSM.Capable {}
  }
  ////////////////////////////////////////////////////////////////////////////////////

}
