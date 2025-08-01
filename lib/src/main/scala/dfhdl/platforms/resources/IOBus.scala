package dfhdl.platforms.resources
import dfhdl.internals.CTName
import dfhdl.compiler.ir.constraints

class IOBus[T <: IO](val ios: T*) extends ResourceDeps:
  def apply(i: Int): T = ios(i)
  lazy val upstreamDeps: List[Resource] = ios.toList
