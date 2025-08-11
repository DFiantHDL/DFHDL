package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.hw.constraints.io

enum SwitchUD extends Encoded.Toggle:
  case Down, Up
object SwitchUD:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: SwitchUD = SwitchUD.Up) extends ToggleIO

enum SwitchRL extends Encoded.Toggle:
  case Left, Right
object SwitchRL:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: SwitchRL = SwitchRL.Right) extends ToggleIO

enum SwitchNS extends Encoded.Toggle:
  case South, North
object SwitchNS:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: SwitchNS = SwitchNS.North) extends ToggleIO

enum SwitchEW extends Encoded.Toggle:
  case West, East
object SwitchEW:
  @io(standard = io.Standard.LVCMOS)
  class Resource(val activeState: SwitchEW = SwitchEW.East) extends ToggleIO
