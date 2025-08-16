package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum SwitchUD extends Encoded.Toggle:
  case Down, Up
object SwitchUD extends ToggleIOComp[SwitchUD](SwitchUD.Up, io.Standard.LVCMOS)

enum SwitchRL extends Encoded.Toggle:
  case Left, Right
object SwitchRL extends ToggleIOComp[SwitchRL](SwitchRL.Right, io.Standard.LVCMOS)

enum SwitchNS extends Encoded.Toggle:
  case South, North
object SwitchNS extends ToggleIOComp[SwitchNS](SwitchNS.North, io.Standard.LVCMOS)

enum SwitchEW extends Encoded.Toggle:
  case West, East
object SwitchEW extends ToggleIOComp[SwitchEW](SwitchEW.East, io.Standard.LVCMOS)
