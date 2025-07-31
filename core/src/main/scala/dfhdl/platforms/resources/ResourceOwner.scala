package dfhdl.platforms.resources
import scala.collection.mutable.ListBuffer
trait ResourceOwner extends ResourceContext:
  dfc.mutableDB.ResourceOwnershipContext.enter(this)
  private val resources = ListBuffer[Resource]()
  private val children = ListBuffer[ResourceOwner]()
  private[resources] def getResources: List[Resource] = resources.toList
  private[resources] def getChildren: List[ResourceOwner] = children.toList
  private[resources] def addResource(resource: Resource): Unit = resources += resource
  override def onCreateEnd(thisOwner: Option[This]): Unit =
    dfc.mutableDB.ResourceOwnershipContext.exit()
    dfc.mutableDB.ResourceOwnershipContext.ownerOpt.foreach(_.children += this)
