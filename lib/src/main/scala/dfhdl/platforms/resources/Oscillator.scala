package dfhdl.platforms.resources
import dfhdl.core.ClkCfg.Rate
import dfhdl.compiler.ir.constraints

final case class Oscillator(
    rate: Rate,
    ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
)(using RCtx) extends HasIOConstraints
