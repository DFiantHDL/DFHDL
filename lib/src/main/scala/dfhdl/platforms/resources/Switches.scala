package dfhdl.platforms.resources
import dfhdl.Encoded

enum SwitchUD extends Encoded.Toggle:
  case Down, Up
object SwitchUD:
  class Resource(val activeState: SwitchUD = SwitchUD.Up) extends ToggleIO

enum SwitchRL extends Encoded.Toggle:
  case Left, Right
object SwitchRL:
  class Resource(val activeState: SwitchRL = SwitchRL.Right) extends ToggleIO

enum SwitchNS extends Encoded.Toggle:
  case South, North
object SwitchNS:
  class Resource(val activeState: SwitchNS = SwitchNS.North) extends ToggleIO

enum SwitchEW extends Encoded.Toggle:
  case West, East
object SwitchEW:
  class Resource(val activeState: SwitchEW = SwitchEW.East) extends ToggleIO
