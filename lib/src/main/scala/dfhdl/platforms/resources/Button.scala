package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button extends ToggleIOComp[Button](Button.Pressed, io.Standard.LVCMOS)
