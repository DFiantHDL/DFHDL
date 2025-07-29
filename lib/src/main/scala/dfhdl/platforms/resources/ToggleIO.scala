package dfhdl.platforms.resources
import dfhdl.Encoded.Toggle

trait ToggleIO[T <: Toggle] extends HasIOConstraints:
  val activeState: T
