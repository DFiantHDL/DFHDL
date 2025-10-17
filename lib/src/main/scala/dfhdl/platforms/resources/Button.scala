package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.timing
import dfhdl.compiler.ir.PhysicalNumber.Ops.Hz

enum Button extends Encoded.Toggle:
  case Released, Pressed

//TODO: No need for `None` when https://github.com/scala/scala3/issues/24201 is fixed
object Button extends ToggleIOComp[Button](Button.Pressed, None, maxFreqMinPeriod = 10.Hz)
