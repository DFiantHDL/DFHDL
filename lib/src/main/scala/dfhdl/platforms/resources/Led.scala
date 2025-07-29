package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.*

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  final case class Resource(
      activeState: Led = Led.Off,
      ioc: io = io(standard = io.Standard.LVCMOS33)
  )(using RCtx) extends ToggleIO
