package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.timing
import dfhdl.compiler.ir.PhysicalNumber.Ops.Hz

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button extends ToggleIOComp[Button](Button.Pressed, maxFreqMinPeriod = 10.Hz)
