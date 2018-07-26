package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFConstruct extends HasProperties with Nameable with TypeNameable with Discoverable {

}

trait DFOwnerConstruct extends DFConstruct {
  protected implicit val blk : this.type = this
  val owner : Option[DFOwnerConstruct]
  final protected implicit val childParent : Option[this.type] = Some(this)

  final protected[DFiant] lazy val protAlmanac = newAlmanac
  final private def newAlmanac : Almanac = {
    owner match {
      case Some(o) =>
        o.protAlmanac.fetchComponent(o.protAlmanac.addComponent(new Almanac(name, Some(o.protAlmanac))))
      case _ =>
        new Almanac(name, None)
    }
  }

  private var idCnt : Int = 0
  final protected[DFiant] def getNewID(run : => Unit) : Int = {run; idCnt += 1; idCnt}

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final def keep : this.type = {
    keepList += this
    this
  }
  protected def discoveryDepenencies : List[Discoverable] = keepList.toList

  final lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name //Top
  }

}