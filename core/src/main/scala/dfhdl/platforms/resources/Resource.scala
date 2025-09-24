package dfhdl.platforms.resources
import scala.annotation.implicitNotFound
import scala.collection.mutable
import dfhdl.compiler.ir.constraints.SigConstraint
import dfhdl.core.*
import dfhdl.internals.*

trait Resource extends ResourceContext:
  private val connections = mutable.ListBuffer[Resource]()
  protected[dfhdl] def connectFrom(that: Resource): Unit =
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
    val visited = mutable.Set[Resource](this)
    val result = mutable.ListBuffer[Resource]()
    def dfs(res: Resource): Unit =
      for (conn <- res.connections)
        if (!visited.contains(conn))
          visited += conn
          result += conn
          dfs(conn)
    dfs(this)
    val res = result.distinct.toList
    res.foreach(checkConnection)
    res
  end allConnections
  protected def checkConnection(res: Resource): Unit = ()
  lazy val allSigConstraints: List[SigConstraint] =
    val all = (this :: allConnections ++ downstreamDeps).flatMap(_.directAndOwnerSigConstraints)
    try all.merge
    catch
      case e: Throwable =>
        def details(res: Resource): Unit =
          val constraints = res.directAndOwnerSigConstraints
          if (constraints.nonEmpty)
            println(s"  ${res.getFullId} with constraints:")
            constraints.foreach(c => println(s"    ${c.toString}"))
          else
            println(s"  ${res.getFullId} with no constraints")
        println("Current resource:")
        details(this)
        println(s"Downstream deps:")
        downstreamDeps.foreach(details)
        println(s"Connections:")
        allConnections.foreach(details)
        throw e
    end try
  end allSigConstraints
  owner match
    case owner: ResourceOwner => owner.addResource(this)
    case _                    =>
end Resource

private trait ResourceLP:
  import Resource.CanConnect
  // connection is commutative, so can connect T to R if can connect R to T
  given [T <: Resource, R <: Resource](using cc: CanConnect[R, T]): CanConnect[T, R] with
    def connect(resource1: T, resource2: R)(using DFC): Unit = cc.connect(resource2, resource1)

object Resource extends ResourceLP:
  @implicitNotFound("Cannot connect the resource ${R} with ${T}")
  trait CanConnect[-R <: Resource, -T]:
    def connect(resource1: R, resource2: T)(using DFC): Unit
  given [R <: Resource, T <: Resource](using R =:= T): CanConnect[R, T] with
    def connect(resource1: R, resource2: T)(using DFC): Unit =
      resource1.connectFrom(resource2)
      resource2.connectFrom(resource1)

  given [R <: Resource, T <: Resource | DFValAny | RTDomainContainer](using
      cc: CanConnect[R, T]
  ): ExactOp2Aux["<>", DFC, Any, R, T, Unit] =
    new ExactOp2["<>", DFC, Any, R, T]:
      type Out = Unit
      def apply(resource1: R, resourceOrValue: T)(using DFC): Out =
        cc.connect(resource1, resourceOrValue)
  end given
  object Ops:
    extension [R <: Resource](resource: R)
      def <>[T <: Resource](that: T)(using dfc: DFC, cc: CanConnect[R, T]): Unit =
        cc.connect(resource, that)
      def <>[T <: DFValAny | RTDomainContainer](that: T)(using
          dfc: DFC,
          cc: CanConnect[R, T]
      ): Unit = trydf {
        cc.connect(resource, that)
      }
  export Resource.Ops.*
end Resource
