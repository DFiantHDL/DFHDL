package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

abstract class DFDesign(
  implicit val parent : Option[DFDesign] = None, val basicLib: DFBasicLib
) extends DFInterface with Implicits {
  protected implicit val dsn = this
  final lazy val components : List[DFDesign] =
    this.getNestedDeclaredFieldsOf[DFDesign](classOf[DFDesign],
      f => f != this, (f, t) => if (!t.hasName) t.setName(f.getName) else t)
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = addDesignToParent
  final protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  def compileToVHDL(fileName : String) = ???
  final protected def newComponent : Almanac = {
    protAlmanac.fetchComponent(new Almanac {})
  }
  final protected def addDesignToParent : Almanac = parent match {
    case Some(p) => p.newComponent
    case _ => {
      setName("top")
      new Almanac {}
    }
  }
  final def isTop : Boolean = parent match {
    case Some(p) => false
    case _ => true
  }

  override def getName: String = {
    parent match {
      case Some(p) => p.components //touching components to force naming
      case _ =>
    }
    super.getName
  }
  override def toString : String = getName
}
object DFDesign {
}

abstract class DFComponent[Comp <: DFComponent[Comp]](
  implicit dsn : DFDesign, impl : DFComponent.Implementation[Comp], basicLib: DFBasicLib
) extends DFDesign()(Some(dsn), basicLib) {
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