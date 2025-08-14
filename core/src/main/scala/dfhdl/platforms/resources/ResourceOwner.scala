package dfhdl.platforms.resources
import scala.collection.mutable.ListBuffer
import dfhdl.compiler.ir

trait ResourceOwner extends ResourceContext:
  dfc.mutableDB.ResourceOwnershipContext.enter(this)
  private val resources = ListBuffer[Resource]()
  private val children = ListBuffer[ResourceOwner]()
  def getResources: List[Resource] = resources.toList
  def getChildren: List[ResourceOwner] = children.toList
  private[resources] def addResource(resource: Resource): Unit = resources += resource
  // projecting global constraints to the current design (typically this would be
  // the top-level design, but we don't limit this for now)
  private def projectGlobalConstraints(): Unit =
    val globalConstraints = getResourceConstraints.collect {
      case constraint: ir.constraints.GlobalConstraint => constraint
    }
    if (globalConstraints.nonEmpty)
      import dfc.getSet
      val ownerDesign = dfc.owner.asIR.getThisOrOwnerDesign
      val updatedDclMeta =
        ownerDesign.dclMeta.copy(annotations = ownerDesign.dclMeta.annotations ++ globalConstraints)
      val updatedOwnerDesign = ownerDesign.copy(dclMeta = updatedDclMeta)
      dfc.mutableDB.replaceMember(ownerDesign, updatedOwnerDesign)
    end if
  end projectGlobalConstraints

  override def onCreateEnd(thisOwner: Option[This]): Unit =
    dfc.mutableDB.ResourceOwnershipContext.exit()
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.foreach(_.children += this)
    projectGlobalConstraints()
end ResourceOwner
