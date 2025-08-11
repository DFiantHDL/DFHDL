package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: Button = Button.Pressed) extends ToggleIO
