package dfhdl.platforms.resources
import dfhdl.internals.CTName
import dfhdl.compiler.ir.constraints

final case class IOBus[T <: IO](val ios: T*)(using RCtx) extends ResourceDeps:
  def apply(i: Int): T = ios(i)
  protected def deps: List[Resource] = ios.toList
