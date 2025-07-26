package dfhdl.platforms.resources
import dfhdl.Encoded

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  final case class Resource(
      activeState: Led = Led.Off,
      level: IOLevel = IOLevel.LVCMOS33
  )(using RCtx) extends ToggleIO
