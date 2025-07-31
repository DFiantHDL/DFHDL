package dfhdl.platforms.resources
import scala.annotation.implicitNotFound
import scala.collection.mutable
import dfhdl.compiler.ir.constraints.SigConstraint

trait Resource extends ResourceContext:
  private val connections = mutable.ListBuffer[Resource]()
  protected[resources] def connect(that: Resource): Unit =
    connections += that
  private lazy val allConnections: List[Resource] =
    val visited = mutable.Set[Resource]()
    val result = mutable.ListBuffer[Resource]()
    def dfs(res: Resource): Unit =
      for (conn <- res.connections)
        if (!visited.contains(conn))
          visited += conn
          result += conn
          dfs(conn)
    dfs(this)
    result.distinct.toList
  end allConnections
  lazy val allSigConstraints: List[SigConstraint] =
    allConnections.flatMap(_.directAndOwnerSigConstraints).distinct
  owner.addResource(this)
end Resource

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
