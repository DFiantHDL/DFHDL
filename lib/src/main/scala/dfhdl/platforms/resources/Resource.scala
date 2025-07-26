package dfhdl.platforms.resources
import scala.annotation.implicitNotFound

trait Resource(using ctx: RCtx):
  final val id: String = ctx.id
  final val owner: ResourceOwner = ctx.owner
  owner.add(this)

private trait ResourceLP:
  import Resource.CanConnect
  given [T <: Resource, R <: Resource](using CanConnect[R, T]): CanConnect[T, R] =
    new CanConnect[T, R] {}

object Resource extends ResourceLP:
  @implicitNotFound("Cannot connect the resources ${T} and ${R}")
  trait CanConnect[T <: Resource, R <: Resource]
  given [T <: Resource, R <: Resource](using T =:= R): CanConnect[T, R] = new CanConnect[T, R] {}

  extension [T <: Resource](self: T)
    def <>[R <: Resource](that: R)(using CanConnect[T, R]): Unit = {}
