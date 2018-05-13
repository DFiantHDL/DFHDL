package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

abstract class DFDesign(
  implicit val parent : Option[DFDesign] = None, val basicLib: DFBasicLib, n : NameIt
) extends DFInterface with Implicits {
  protected implicit val dsn = this
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = addDesignToParent

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final protected def setComponentScalaNames : Unit = {

  }
  final protected lazy val components : List[DFDesign] =
  this.getNestedDeclaredFieldsOf[DFDesign](classOf[DFDesign],
    f => f != this, (f, t) => if (!t.hasName) t.setAutoName(f.getName) else t)

  final protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  final protected def newComponent(comp : DFDesign) : Almanac = {
    protAlmanac.fetchComponent(protAlmanac.addComponent(new Almanac {}).setAutoName(getName))
  }
  final protected def addDesignToParent : Almanac = {
    val ret = parent match {
      case Some(p) => p.newComponent(this)
      case _ => {
        setAutoName("top")
        new Almanac {}
      }
    }
    println("hi")
    ret
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???
  def keep : this.type = {
    protAlmanac //touching lazy Almanac
    this
  }
  final def isTop : Boolean = parent match {
    case Some(p) => false
    case _ => true
  }

  override def setName(name: String): DFDesign.this.type = {
    protAlmanac.setName(name)
    super.setName(name)
  }

  override def getName: String = {
    parent match {
      case Some(p) => p.components //touching components to force naming
      case _ =>
    }
    super.getName
  }
  setAutoName(n.value)
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