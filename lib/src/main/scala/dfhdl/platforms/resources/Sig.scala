package dfhdl.platforms.resources
import dfhdl.compiler.ir.constraints

final case class Sig(
    ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
)(using RCtx) extends HasIOConstraints
