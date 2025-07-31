package dfhdl.platforms.resources
import dfhdl.Encoded

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  class Resource(val activeState: Led = Led.Off) extends ToggleIO
