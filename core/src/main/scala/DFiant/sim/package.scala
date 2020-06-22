package DFiant

import sim.DFSimMember.{Assert, Finish}
import Assert.{Message}


package object sim {
  ////////////////////////////////////////////////////////////////////////////////////
  // Simulation-related constructs
  ////////////////////////////////////////////////////////////////////////////////////
  def inSimulation(implicit ctx : DFAny.Context) : Boolean = ctx.db.top.simMode match {
    case DFSimDesign.Mode.Off => false
    case DFSimDesign.Mode.On => true
  }

  def assert[C](cond : C, msg : Message, severity : Severity = Warning)(
    implicit ctx : DFAny.Context, condArg : DFBool.Arg[0]
  ) : Unit = {
    if (inSimulation) Assert(Some(condArg()), msg, severity)
  }
  def report(msg : Message, severity : Severity = Note)(implicit ctx : DFAny.Context) : Unit = {
    if (inSimulation) Assert(None, msg, severity)
  }
  def finish()(implicit ctx : DFAny.Context) : Unit = {
    if (inSimulation) Finish()
  }
  ////////////////////////////////////////////////////////////////////////////////////

}