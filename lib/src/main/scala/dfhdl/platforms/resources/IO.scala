package dfhdl.platforms.resources
import dfhdl.platforms.resources.Resource.CanConnect

trait IO extends Resource
object IO:
  given [T <: IO, R <: IO]: CanConnect[T, R] = new CanConnect[T, R] {}
