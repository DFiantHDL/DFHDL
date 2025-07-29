package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.compiler.ir.constraints

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  final case class Resource(
      activeState: Led = Led.Off,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx) extends ToggleIO
