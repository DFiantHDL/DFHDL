package dfhdl.platforms.resources
import dfhdl.Encoded

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button:
  final case class Resource(
      activeState: Button = Button.Pressed,
      level: IOLevel = IOLevel.LVCMOS33
  )(using RCtx) extends ToggleIO
