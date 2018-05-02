package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

abstract class DFDesign(
  implicit val parent : Option[DFDesign] = None, val basicLib: DFBasicLib
) extends DFInterface with Implicits {
  protected implicit val dsn = this
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = addDesignToParent
  final protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  def compileToVHDL(fileName : String) = ???
  final protected def addChildDesign(childDsn : DFDesign) : Almanac = protAlmanac.fetchComponent(new Almanac {})
  final protected def addDesignToParent : Almanac = parent match {
    case Some(p) => p.addChildDesign(this)
    case _ => new Almanac {}
  }
  final def isTop : Boolean = parent match {
    case Some(p) => false
    case _ => true
  }
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

abstract class RTComponent(implicit dsn : DFDesign) extends DFInterface {
  protected def newGeneric() : Unit = {}

  dsn.addRTComponent(this)
}