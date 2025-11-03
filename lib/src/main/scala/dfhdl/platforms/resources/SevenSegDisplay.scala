package dfhdl.platforms.resources
import dfhdl.{DFC, Encoded}
import dfhdl.hw.constraints.*
import SevenSegDisplay.{Segment, Select}
import dfhdl.compiler.ir.PhysicalNumber.Ops.us
import dfhdl.compiler.ir.constraints.{IO, Timing}

class SevenSegDisplay[L <: Int & Singleton](
    val digits: L,
    val selectActiveState: Select,
    val segmentActiveState: Segment
) extends ResourceGroup:
  val SELECT = IOBus.fill(digits)(Select(selectActiveState))
  val A = Segment(segmentActiveState)
  val B = Segment(segmentActiveState)
  val C = Segment(segmentActiveState)
  val D = Segment(segmentActiveState)
  val E = Segment(segmentActiveState)
  val F = Segment(segmentActiveState)
  val G = Segment(segmentActiveState)
  val DP = Segment(segmentActiveState)
end SevenSegDisplay
object SevenSegDisplay:
  enum Segment extends Encoded.Toggle:
    case Off, On
  object Segment:
    @io(dir = io.Dir.OUT, standard = io.Standard.LVCMOS, slewRate = io.SlewRate.SLOWEST)
    @timing.ignore(maxFreqMinPeriod = 200.us)
    protected[SevenSegDisplay] class Resource private[Segment] (val activeState: Segment)
        extends ToggleIO[Segment]
    protected[SevenSegDisplay] def apply(activeState: Segment)(using DFC): Resource =
      new Resource(activeState)
  enum Select extends Encoded.Toggle:
    case Disabled, Enabled
  object Select:
    protected[SevenSegDisplay] class Resource private[Select] (val activeState: Select)
        extends ToggleIO[Select]:
      private val pullMode = if (activeState == Select.Enabled) IO.PullMode.DOWN else IO.PullMode.UP
      injectConstraint(IO(
        dir = IO.Dir.OUT,
        standard = IO.Standard.LVCMOS,
        slewRate = IO.SlewRate.SLOWEST,
        unusedPullMode = pullMode
      ))
      injectConstraint(Timing.Ignore(maxFreqMinPeriod = 200.us))
    protected[SevenSegDisplay] def apply(activeState: Select)(using DFC): Resource =
      new Resource(activeState)
end SevenSegDisplay
