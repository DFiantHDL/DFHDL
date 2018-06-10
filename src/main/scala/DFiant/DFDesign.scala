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

  protected def implementation() : Unit

//  final lazy val namedNonPorts : List[DFAny] =
//    this.getNestedDeclaredFieldsOf[DFAny](classOf[DFAny],
//      t => !t.isPort, (f, t) => if (!t.hasName) t.setAutoName(f.getName) else t)


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] val components : ListBuffer[DFDesign] = ListBuffer.empty[DFDesign]
//  final protected[DFiant] lazy val namedComponents : List[DFDesign] =
//    this.getNestedDeclaredFieldsOf[DFDesign](classOf[DFDesign], f => f != this, (f, t) => {if (f.getName != "dsn") t.setAutoName(f.getName); t})

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
//        o.namedComponents
//        o.namedNonPorts
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

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  final protected[DFiant] def newDFVal(dfval : DFAny) : Unit = {
    dfvals += dfval
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  def compileToVHDL(fileName : String) = ???
  def keep : this.type = {
    keepList += this //touching lazy Almanac
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

  protected def discoveryDepenencies : List[Discoverable] = portsOut ++ keepList //components.toList ++ dfvals ++ ports//
  protected def discovery : Unit = protAlmanac

  protected lazy val init : Unit = {
//    namedComponents
//    namedNonPorts
    //Run init of all components
    components.foreach(c => c.init)
    implementation()
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  setAutoName(n.value)
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

abstract class RTComponent(implicit dsn : DFDesign, n : NameIt) extends DFInterface {
  protected def newGeneric() : Unit = {}

  dsn.addRTComponent(this)
}