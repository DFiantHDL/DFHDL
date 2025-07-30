package dfhdl.platforms.resources
import scala.annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

trait Resource(using ctx: RCtx):
  final val id: String = ctx.id
  final val owner: ResourceOwner = ctx.owner
  private val connections = ListBuffer[Resource]()
  protected[resources] def connect(that: Resource): Unit =
    connections += that
  owner.add(this)

private trait ResourceLP:
  import Resource.CanConnect
  // connection is commutative, so can connect T to R if can connect R to T
  given [T <: Resource, R <: Resource](using cc: CanConnect[R, T]): CanConnect[T, R] =
    (resource1: T, resource2: R) =>
      cc.connect(resource2, resource1)

object Resource extends ResourceLP:
  @implicitNotFound("Cannot connect the resources ${T} and ${R}")
  trait CanConnect[T <: Resource, R <: Resource]:
    def connect(resource1: T, resource2: R): Unit
  given [T <: Resource, R <: Resource](using T =:= R): CanConnect[T, R] =
    (resource1: T, resource2: R) =>
      resource1.connect(resource2)
      resource2.connect(resource1)

  extension [T <: Resource](self: T)
    def <>[R <: Resource](that: R)(using cc: CanConnect[T, R]): Unit =
      cc.connect(self, that)
