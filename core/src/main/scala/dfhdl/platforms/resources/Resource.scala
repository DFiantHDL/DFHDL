package dfhdl.platforms.resources
import scala.annotation.implicitNotFound
import scala.collection.mutable
import dfhdl.compiler.ir.constraints.SigConstraint
import dfhdl.core.*
import dfhdl.internals.CTName

trait Resource extends ResourceContext:
  private val connections = mutable.ListBuffer[Resource]()
  protected[resources] def connect(that: Resource): Unit =
    connections += that
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
    allConnections.flatMap(_.directAndOwnerSigConstraints).merge
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
  trait CanConnect[T <: Resource, R]:
    def connect(resource1: T, resource2: R): Unit
  given [T <: Resource, R <: Resource](using T =:= R): CanConnect[T, R] =
    (resource1: T, resource2: R) =>
      resource1.connect(resource2)
      resource2.connect(resource1)
  given [T <: Resource, R <: DFValAny](using DFC): CanConnect[T, R] =
    (resource: T, dfVal: R) =>
      given CTName = CTName("<>")
      trydf { dfVal.connect(resource) }

  object Ops:
    extension [T <: Resource](self: T)
      def <>[R](that: R)(using cc: CanConnect[T, R]): Unit =
        cc.connect(self, that)
end Resource
