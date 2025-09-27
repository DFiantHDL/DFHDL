package dfhdl.platforms.resources
import dfhdl.internals.*
import dfhdl.core.*
import scala.annotation.Annotation
import dfhdl.compiler.ir.constraints.{Constraint, SigConstraint}
import scala.collection.mutable.ListBuffer

trait ResourceContext extends OnCreateEvents, HasDFC, HasClsMetaArgs:
  private var _injectedID: Option[String] = None
  private[resources] def injectID(id: String): this.type =
    _injectedID = Some(id)
    this
  final lazy val dfc: DFC = __dfc
  final lazy val id: String = _injectedID.getOrElse(dfc.nameOpt.getOrElse("anon"))
  private var resourceType: String = ""
  def getResourceType: String = resourceType
  def getFullId: String =
    if (isTopResource) id
    else owner.getFullId + "." + id
  // the top-level resource owner is set to be itself
  private[resources] val owner: ResourceContext =
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.getOrElse(this)
  private[resources] val isTopResource: Boolean =
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.isEmpty
  private var resourceConstraints = ListBuffer.from(dfc.annotations.collect {
    case constraint: Constraint => constraint
  })
  def getResourceConstraints: List[Constraint] = resourceConstraints.toList
  // the constraints that are applied to this resource and its owners (including connections for groups)
  private[resources] lazy val directAndOwnerSigConstraints: List[SigConstraint] =
    val ownerConstraints =
      if (isTopResource) Nil
      else owner match
        case group: ResourceGroup => group.directAndOwnerSigConstraints ++ group.allSigConstraints
        case owner                => owner.directAndOwnerSigConstraints
    (ownerConstraints ++ getResourceConstraints.collect {
      case constraint: SigConstraint => constraint
    }).merge
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  final protected[resources] def injectConstraint(constraint: Constraint): this.type =
    resourceConstraints += constraint
    this
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit =
    resourceType = name
    annotations.foreach {
      case constraint: dfhdl.hw.constraints.Constraint =>
        resourceConstraints += constraint.asIR
      case _ =>
    }
end ResourceContext
