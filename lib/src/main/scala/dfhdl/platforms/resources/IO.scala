package dfhdl.platforms.resources
import dfhdl.platforms.resources.Resource.CanConnect
import dfhdl.compiler.ir.constraints

trait IO extends Resource
object IO:
  given [T <: IO, R <: IO]: CanConnect[T, R] = (resource1: T, resource2: R) =>
    resource1.connect(resource2)
    resource2.connect(resource1)

trait HasIOConstraints extends IO:
  val ioc: constraints.IO
