package dfhdl.platforms.resources
import dfhdl.core.ClkCfg.Rate
import dfhdl.hw.constraints.*

final case class Oscillator(
    rate: Rate,
    ioc: io = io(standard = io.Standard.LVCMOS33)
)(using RCtx) extends HasIOConstraints
