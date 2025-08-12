package dfhdl.platforms.resources
import dfhdl.core.ClkCfg.Rate
import Resource.CanConnect
import dfhdl.*
import dfhdl.compiler.ir.constraints.Timing

class Oscillator(val rate: Rate) extends IO:
  injectConstraint(Timing.Clock(rate))
object Oscillator:
  given [O <: Oscillator, C <: (Clk <> VAL)]: CanConnect[O, C] = (o, c) => o.connect(c)
