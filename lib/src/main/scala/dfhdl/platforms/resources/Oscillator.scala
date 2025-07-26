package dfhdl.platforms.resources
import dfhdl.core.ClkCfg.Rate

final case class Oscillator(rate: Rate, level: IOLevel)(using RCtx) extends HasIOLevel
