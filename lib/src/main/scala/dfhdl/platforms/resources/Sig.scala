package dfhdl.platforms.resources
import dfhdl.hw.constraints.*

final case class Sig(
    ioc: io = io(standard = io.Standard.LVCMOS33)
)(using RCtx) extends HasIOConstraints
