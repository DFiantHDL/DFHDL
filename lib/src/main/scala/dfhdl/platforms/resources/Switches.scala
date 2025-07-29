package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.compiler.ir.constraints

enum SwitchUD extends Encoded.Toggle:
  case Down, Up
object SwitchUD:
  final case class Resource(
      activeState: SwitchUD = SwitchUD.Up,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx)
      extends ToggleIO

enum SwitchRL extends Encoded.Toggle:
  case Left, Right
object SwitchRL:
  final case class Resource(
      activeState: SwitchRL = SwitchRL.Right,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx)
      extends ToggleIO

enum SwitchNS extends Encoded.Toggle:
  case South, North
object SwitchNS:
  final case class Resource(
      activeState: SwitchNS = SwitchNS.North,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx)
      extends ToggleIO

enum SwitchEW extends Encoded.Toggle:
  case West, East
object SwitchEW:
  final case class Resource(
      activeState: SwitchEW = SwitchEW.East,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx)
      extends ToggleIO
