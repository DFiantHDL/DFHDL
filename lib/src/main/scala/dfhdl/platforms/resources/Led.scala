package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.compiler.ir.constraints.IO

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  class Resource(val activeState: Led = Led.On) extends ToggleIO:
    private val pullMode = if (activeState == Led.On) IO.PullMode.DOWN else IO.PullMode.UP
    injectConstraint(IO(standard = IO.Standard.LVCMOS, unusedPullMode = pullMode))
