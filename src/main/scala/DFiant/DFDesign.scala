package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class DFDesign(
  implicit val owner : Option[DFDesign] = None, val basicLib: DFBasicLib, n : NameIt
) extends DFInterface with Implicits with Discoverable {
  protected implicit val dsn = this
  final val topDsn : DFDesign = owner match {
    case Some(o) => o.topDsn
    case _ => this
  }
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = newAlmanac

  protected def implementation() : Unit

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] val components : ListBuffer[DFDesign] = ListBuffer.empty[DFDesign]

  final protected[DFiant] def newRTComponent(comp : RTComponent) : Unit = {}
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
  protected val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final protected[DFiant] def newDFValGetID(dfval : DFAny) : Int = {
    dfvals += dfval
    dfvals.size
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  def compileToVHDL(fileName : String) = ???
  def keep : this.type = {
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
  override protected def nameDefault: String = if (n.value == "$anon") "top" else n.value
  override def setName(name: String): DFDesign.this.type = {
    protAlmanac.setName(name)
    super.setName(name)
  }

  lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name
  }

  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected def discoveryDepenencies : List[Discoverable] = portsOut ++ keepList //components.toList ++ dfvals ++ ports//
  protected def discovery : Unit = protAlmanac

  protected lazy val init : Unit = {
    //Run init of all components
    components.foreach(c => c.init)
    implementation()
  }

  def printInfo() : Unit = {
    init
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
  def implementation(): Unit = {
    impl(this.asInstanceOf[Comp])
  }
}

object DFComponent {
  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

