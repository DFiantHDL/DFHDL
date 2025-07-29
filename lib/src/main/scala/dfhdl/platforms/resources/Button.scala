package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.compiler.ir.constraints

enum Button extends Encoded.Toggle:
  case Released, Pressed

object Button:
  final case class Resource(
      activeState: Button = Button.Pressed,
      ioc: constraints.IO = constraints.IO(standard = constraints.IO.Standard.LVCMOS33)
  )(using RCtx) extends ToggleIO
