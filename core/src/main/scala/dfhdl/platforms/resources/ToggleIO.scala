package dfhdl.platforms.resources
import dfhdl.Encoded.Toggle
import dfhdl.core.*
import Resource.CanConnect
import dfhdl.internals.*
import dfhdl.compiler.ir.constraints

trait ToggleIO[T <: Toggle] extends IO:
  val activeState: T

object ToggleIO:
  given [T <: Toggle, R <: ToggleIO[T], V <: DFValOf[DFBoolOrBit]](using
      dfc: DFC,
      t: ShowType[T]
  )(using
      expectedActiveState: GivenOrError[
        ExpectedActiveState[T],
        "Missing implicit expected active state for toggle resource `" +
          t.Out +
          "`.\nTo fix this, add:\n  `given ExpectedActiveState[" + t.Out + "] = " + t.Out + ".EXPECTED_ACTIVE_STATE`"
      ]
  ): CanConnect[R, V] = (resource: R, dfVal: V) =>
    resource.injectConstraint(constraints.IO(invertActiveState =
      resource.activeState != expectedActiveState.value
    )).connect(dfVal)
end ToggleIO

opaque type ExpectedActiveState[T <: Toggle] <: T = T
object ExpectedActiveState:
  given [T <: Toggle]: Conversion[T, ExpectedActiveState[T]] = identity
