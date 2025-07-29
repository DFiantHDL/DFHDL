package dfhdl.platforms.resources
import dfhdl.platforms.resources.Resource.CanConnect
import dfhdl.hw.constraints.*

trait IO extends Resource
object IO:
  given [T <: IO, R <: IO]: CanConnect[T, R] = new CanConnect[T, R] {}

trait HasIOConstraints extends IO:
  val ioc: io
