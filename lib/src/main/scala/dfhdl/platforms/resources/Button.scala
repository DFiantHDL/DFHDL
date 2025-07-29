package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.*

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button:
  final case class Resource(
      activeState: Button = Button.Pressed,
      ioc: io = io(standard = io.Standard.LVCMOS33)
  )(using RCtx) extends ToggleIO
