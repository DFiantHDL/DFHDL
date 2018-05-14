package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFDesign(
  implicit val owner : Option[DFDesign] = None, val basicLib: DFBasicLib, n : NameIt
) extends DFInterface with Implicits with Discoverable {
  protected implicit val dsn = this
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = newAlmanac

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] val components : ListBuffer[DFDesign] = ListBuffer.empty[DFDesign]
  final protected[DFiant] lazy val namedComponents : List[DFDesign] =
    this.getNestedDeclaredFieldsOf[DFDesign](classOf[DFDesign], f => f != this, (f, t) => t.setAutoName(f.getName))

  final protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  final protected def newComponent(comp : DFDesign) : Unit = {
    components += comp
  }

  final protected def addComponentToParent : Unit = {
    owner match {
      case Some(o) => o.newComponent(this)
      case _ =>
    }
  }
  final protected def newAlmanac : Almanac = {
    owner match {
      case Some(o) =>
        o.namedComponents
        o.protAlmanac.fetchComponent(o.protAlmanac.addComponent(new Almanac {}.setName(getName)))
      case _ =>
        setAutoName(if (n.value == "$anon") "top" else n.value)
        new Almanac {}.setName(getName)
    }
  }
  final protected def printComponents() : Unit = {
    components.foreach(c => println(c.getName))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???
  def keep : this.type = {
    protAlmanac //touching lazy Almanac
    this
  }
  final def isTop : Boolean = owner match {
    case Some(o) => false
    case _ => true
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def setName(name: String): DFDesign.this.type = {
    protAlmanac.setName(name)
    super.setName(name)
  }

  override def getName: String = {
    owner match {
      case Some(o) => o.components //touching components to force naming
      case _ =>
    }
    super.getName
  }

  def getFullName : String = owner match {
    case Some(o) => s"${o.getFullName}_$getName"
    case _ => getName
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected def discoveryDepenencies : List[Discoverable] = portsOut
  protected def discovery : Unit = protAlmanac

  def printInfo() : Unit = {
    discover
    protAlmanac.printInfo()
  }

  addComponentToParent
}
object DFDesign {
}

abstract class DFComponent[Comp <: DFComponent[Comp]](
  implicit dsn : DFDesign, impl : DFComponent.Implementation[Comp], basicLib: DFBasicLib, n : NameIt
) extends DFDesign()(Some(dsn), basicLib, n) {
  impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

abstract class RTComponent(implicit dsn : DFDesign, n : NameIt) extends DFInterface {
  protected def newGeneric() : Unit = {}

  dsn.addRTComponent(this)
}