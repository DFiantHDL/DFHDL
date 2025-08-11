package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: Led = Led.Off) extends ToggleIO
