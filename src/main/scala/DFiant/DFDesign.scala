package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFDesign(
  implicit val owner : Option[DFDesign] = None, val basicLib: DFBasicLib, n : NameIt
) extends DFInterface with Implicits {
  protected implicit val dsn = this
  final val topDsn : DFDesign = owner match {
    case Some(o) => o.topDsn
    case _ => this
  }
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = newAlmanac

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final protected[DFiant] val components : ListBuffer[DFDesign] = ListBuffer.empty[DFDesign]
  final protected[DFiant] val rtcomponents : ListBuffer[RTComponent] = ListBuffer.empty[RTComponent]

  final protected def newComponentGetID(comp : DFDesign) : Int = {
    components += comp
    components.size
  }
  final protected[DFiant] def newRTComponentGetID(comp : RTComponent) : Int = {
    rtcomponents += comp
    rtcomponents.size
  }

  final protected def addComponentToParentGetID : Int = {
    owner match {
      case Some(o) => o.newComponentGetID(this)
      case _ => 0
    }
  }
  final protected def newAlmanac : Almanac = {
    owner match {
      case Some(o) =>
        o.protAlmanac.fetchComponent(o.protAlmanac.addComponent(new Almanac {}.setName(name)))
      case _ =>
        new Almanac {}.setName(name)
    }
  }
  final protected def printComponents() : Unit = {
    components.foreach(c => println(c.name))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final protected val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final protected[DFiant] def newDFValGetID(dfval : DFAny) : Int = {
    dfvals += dfval
    dfvals.size
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  def compileToVHDL(fileName : String) = ???
  final def keep : this.type = {
    keepList += this
    this
  }
  final def isTop : Boolean = owner match {
    case Some(o) => false
    case _ => true
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final override protected def nameDefault: String = if (n.value == "$anon") "top" else n.value
  final override def setName(name: String): DFDesign.this.type = {
    protAlmanac.setName(name)
    super.setName(name)
  }

  final lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name
  }

  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def discoveryDepenencies : List[Discoverable] = portsOut ++ keepList //components.toList ++ dfvals ++ ports//
  final protected def discovery : Unit = protAlmanac

  final protected lazy val init : Unit = {
    //Run init of all rtcomponents
    rtcomponents.foreach(c => c.init)
    //Run init of all components
    components.foreach(c => c.init)
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  final val id = addComponentToParentGetID
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

