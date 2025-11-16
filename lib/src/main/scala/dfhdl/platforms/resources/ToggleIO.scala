package dfhdl.platforms.resources
import dfhdl.Encoded.Toggle
import dfhdl.core.*
import Resource.CanConnect
import dfhdl.internals.*
import dfhdl.compiler.ir.{constraints, ConfigN, RateNumber}

trait ToggleIO[T <: Toggle] extends IO:
  val activeState: T

object ToggleIO:
  given [T <: Toggle, R <: ToggleIO[T], V <: DFValOf[DFBoolOrBit]](using
      t: ShowType[T]
  )(using
      expectedActiveState: GivenOrError[
        ExpectedActiveState[T],
        "Missing implicit expected active state for toggle resource `" +
          t.Out +
          "`.\nTo fix this, add:\n  `given ExpectedActiveState[" + t.Out + "] = " + t.Out + ".EXPECTED_ACTIVE_STATE`"
      ]
  ): CanConnect[R, V] with
    def connect(resource: R, dfVal: V)(using DFC): Unit =
      resource.injectConstraint(constraints.IO(invertActiveState =
        resource.activeState != expectedActiveState.value
      )).connect(dfVal)
  end given
end ToggleIO

abstract class ToggleIOComp[T <: Toggle](
    defaultActiveState: => T,
    standard: ConfigN[constraints.IO.Standard] = None,
    maxFreqMinPeriod: ConfigN[RateNumber] = None
):
  class Resource private[ToggleIOComp] (val activeState: T)
      extends ToggleIO[T]:
    injectConstraint(constraints.IO(standard = standard, dir = constraints.IO.Dir.IN))
    if (maxFreqMinPeriod != None)
      injectConstraint(constraints.Timing.Ignore(maxFreqMinPeriod = maxFreqMinPeriod))
  def apply(activeState: T = defaultActiveState)(using DFC): Resource = new Resource(activeState)

opaque type ExpectedActiveState[T <: Toggle] <: T = T
object ExpectedActiveState:
  given [T <: Toggle]: Conversion[T, ExpectedActiveState[T]] = identity
