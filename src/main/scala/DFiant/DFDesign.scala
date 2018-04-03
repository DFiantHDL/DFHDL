package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

abstract class DFDesign(implicit val parent : Option[DFDesign] = None, val basicLib: DFBasicLib) extends DFInterface with Implicits {
  protected implicit val dsn = this
  protected implicit val childParent = Some(this)
  protected[DFiant] val protAlmanac = new Almanac {}
  protected[DFiant] def addRTComponent(comp : RTComponent) : Unit = {}
  def compileToVHDL(fileName : String) = ???
  protected def addChildDesign(childDsn : DFDesign) = {}
  protected def addDesignToParent = parent match {
    case Some(p) => p.addChildDesign(this)
    case _ => //No parent => this is the TOP design
  }
  def isTop : Boolean = parent match {
    case Some(p) => false
    case _ => true
  }
  addDesignToParent
}
object DFDesign {
}

abstract class DFComponent[Comp <: DFComponent[Comp]](implicit dsn : DFDesign, impl : DFComponent.Implementation[Comp], basicLib: DFBasicLib) extends DFDesign()(Some(dsn), basicLib) {
  impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

abstract class RTComponent(implicit dsn : DFDesign) extends DFInterface {
  dsn.addRTComponent(this)
}