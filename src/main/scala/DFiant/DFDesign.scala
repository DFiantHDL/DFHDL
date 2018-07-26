package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.ListBuffer

sealed abstract class DFBlock(
  implicit val owner : Option[DFBlock] = None, val basicLib: DFBasicLib, n : NameIt
) extends DFOwnerConstruct with Implicits {
  final val topDsn : DFBlock = owner match {
    case Some(o) => o.topDsn
    case _ => this
  }
  final protected implicit val childParent = Some(this)
  final protected[DFiant] lazy val protAlmanac = newAlmanac

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Sub-Blocks
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val blocks : ListBuffer[DFBlock] = ListBuffer.empty[DFBlock]
  final private val rtcomponents : ListBuffer[RTComponent] = ListBuffer.empty[RTComponent]

  final private def newBlockGetID(comp : DFBlock) : Int = getNewID(blocks += comp)
  final private[DFiant] def newRTComponentGetID(comp : RTComponent) : Int = getNewID(rtcomponents += comp)

  final private def addBlockToOwnerGetID : Int = {
    owner match {
      case Some(o) => o.newBlockGetID(this)
      case _ => 0
    }
  }
  final private def newAlmanac : Almanac = {
    owner match {
      case Some(o) =>
        o.protAlmanac.fetchComponent(o.protAlmanac.addBlock(new Almanac(name, Some(o.protAlmanac))))
      case _ =>
        new Almanac(name, None)
    }
  }
  final protected def printBlocks() : Unit = {
    blocks.foreach(c => println(c.name))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final private[DFiant] def newDFValGetID(dfval : DFAny) : Int = getNewID(dfvals += dfval)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???
  final def isTop : Boolean = owner.isEmpty

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final override protected def nameDefault: String = if (isTop && n.value == "$anon") "top" else n.value
  final lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name //Top
  }

  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def discovery : Unit = protAlmanac

  final protected lazy val init : Unit = {
    //Run init of all rtcomponents
    rtcomponents.foreach(c => c.init)
    //Run init of all components
    blocks.foreach(c => c.init)
  }

  def codeString : String = {
    init
    discover
    protAlmanac.codeString
  }

  def printInfo() : Unit = {
    init
    discover
    protAlmanac.printInfo()
  }

  final val id = addBlockToOwnerGetID
}
object DFBlock {
}

abstract class DFDesign(implicit owner : Option[DFBlock] = None, basicLib: DFBasicLib, n : NameIt
) extends DFBlock with DFInterface {
  final override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies
}

abstract class DFComponent[Comp <: DFComponent[Comp]](
  implicit blk : DFBlock, impl : DFComponent.Implementation[Comp], basicLib: DFBasicLib, n : NameIt
) extends DFDesign()(Some(blk), basicLib, n) {
  impl(this.asInstanceOf[Comp])
}

object DFComponent {
  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

