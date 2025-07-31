package dfhdl.platforms.resources
import dfhdl.Encoded

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button:
  class Resource(val activeState: Button = Button.Pressed) extends ToggleIO
