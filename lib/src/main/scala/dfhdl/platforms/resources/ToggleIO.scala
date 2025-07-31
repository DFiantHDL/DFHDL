package dfhdl.platforms.resources
import dfhdl.Encoded.Toggle

trait ToggleIO[T <: Toggle] extends IO:
  val activeState: T
