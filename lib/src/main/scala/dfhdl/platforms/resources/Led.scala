package dfhdl.platforms.resources
import dfhdl.Encoded
import dfhdl.compiler.ir.constraints.{IO, Timing}
import dfhdl.compiler.ir.PhysicalNumber.Ops.Hz
import dfhdl.core.DFC

enum Led extends Encoded.Toggle:
  case Off, On
object Led:
  class Resource private[Led] (val activeState: Led = Led.On) extends ToggleIO[Led]:
    private val pullMode = if (activeState == Led.On) IO.PullMode.DOWN else IO.PullMode.UP
    injectConstraint(IO(
      standard = IO.Standard.LVCMOS,
      slewRate = IO.SlewRate.SLOWEST,
      unusedPullMode = pullMode
    ))
    injectConstraint(Timing.Ignore(maxFreqMinPeriod = 100.Hz))
  def apply(activeState: Led = Led.On)(using DFC): Resource = new Resource(activeState)
