package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum SwitchUD extends Encoded.Toggle:
  case Down, Up
object SwitchUD extends ToggleIOComp[SwitchUD](SwitchUD.Up)

enum SwitchRL extends Encoded.Toggle:
  case Left, Right
object SwitchRL extends ToggleIOComp[SwitchRL](SwitchRL.Right)

enum SwitchNS extends Encoded.Toggle:
  case South, North
object SwitchNS extends ToggleIOComp[SwitchNS](SwitchNS.North)

enum SwitchEW extends Encoded.Toggle:
  case West, East
object SwitchEW extends ToggleIOComp[SwitchEW](SwitchEW.East)
