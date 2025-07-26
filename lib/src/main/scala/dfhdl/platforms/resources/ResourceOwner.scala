package dfhdl.platforms.resources
import scala.collection.mutable.ListBuffer
trait ResourceOwner:
  protected given ResourceOwner = this
  private val resources = ListBuffer[Resource]()
  def add(resource: Resource): Unit = resources += resource

sealed trait NoResourceOwner extends ResourceOwner
case object NoResourceOwner extends NoResourceOwner
