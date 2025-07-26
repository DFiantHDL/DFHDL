package dfhdl.platforms.resources
import dfhdl.Encoded.Toggle

trait ToggleIO[T <: Toggle] extends HasIOLevel:
  val activeState: T
