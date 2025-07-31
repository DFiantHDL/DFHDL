package dfhdl.platforms.resources
import dfhdl.internals.*
import dfhdl.core.*
import scala.annotation.Annotation
import dfhdl.compiler.ir.constraints.{Constraint, SigConstraint}
import scala.collection.mutable.ListBuffer

trait ResourceContext extends OnCreateEvents, HasDFC, HasClsMetaArgs:
  final lazy val dfc: DFC = __dfc
  final lazy val id: String = dfc.nameOpt.get
  // the top-level resource owner is set to be itself
  private[resources] val owner: ResourceOwner =
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.getOrElse(this.asInstanceOf[ResourceOwner])
  private[resources] val isTopResource: Boolean =
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.isEmpty
  private var resourceConstraints = ListBuffer.from(dfc.annotations.collect {
    case constraint: Constraint => constraint
  })
  def getResourceConstraints: List[Constraint] = resourceConstraints.toList
  // the constraints that are directly applied to this resource and its owners
  private[resources] lazy val directAndOwnerSigConstraints: List[SigConstraint] =
    val ownerConstraints = if (isTopResource) Nil else owner.directAndOwnerSigConstraints
    (ownerConstraints ++ getResourceConstraints.collect {
      case constraint: SigConstraint => constraint
    }).merge
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  protected def injectConstraint(constraint: Constraint): Unit =
    resourceConstraints += constraint
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit =
    annotations.foreach {
      case constraint: dfhdl.hw.constraints.Constraint =>
        resourceConstraints += constraint.asIR
      case _ =>
    }
end ResourceContext
