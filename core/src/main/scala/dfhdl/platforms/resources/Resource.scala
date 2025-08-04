package dfhdl.platforms.resources
import scala.annotation.implicitNotFound
import scala.collection.mutable
import dfhdl.compiler.ir.constraints.SigConstraint
import dfhdl.core.*
import dfhdl.internals.*

trait Resource extends ResourceContext:
  private val connections = mutable.ListBuffer[Resource]()
  protected[resources] def connect(that: Resource): Unit =
    connections += that
  protected[dfhdl] def connect(that: DFValAny)(using dfc: DFC): Unit =
    throw new IllegalArgumentException(
      "Cannot connect this resource to that DFHDL value."
    )
  private val downstreamDeps = mutable.ListBuffer[ResourceDeps]()
  private[resources] def addDownstreamDep(that: ResourceDeps): Unit =
    downstreamDeps += that
  // will always return at least this resource
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
    val res = result.distinct.toList
    if (res.isEmpty) List(this) else res
  end allConnections
  lazy val allSigConstraints: List[SigConstraint] =
    (allConnections ++ downstreamDeps).flatMap(_.directAndOwnerSigConstraints).merge
  owner.addResource(this)
end Resource

private trait ResourceLP:
  import Resource.CanConnect
  // connection is commutative, so can connect T to R if can connect R to T
  given [T <: Resource, R <: Resource](using cc: CanConnect[R, T]): CanConnect[T, R] =
    (resource1: T, resource2: R) =>
      cc.connect(resource2, resource1)

object Resource extends ResourceLP:
  @implicitNotFound("Cannot connect the resource ${T} with ${R}")
  trait CanConnect[R <: Resource, T]:
    def connect(resource1: R, resource2: T): Unit
  given [R <: Resource, T <: Resource](using R =:= T): CanConnect[R, T] =
    (resource1: R, resource2: T) =>
      resource1.connect(resource2)
      resource2.connect(resource1)

  object Ops:
    extension [R <: Resource](resource: R)
      def <>[T](that: T)(using dfc: DFC, cc: CanConnect[R, T]): Unit = trydf {
        cc.connect(resource, that)
      }
end Resource
