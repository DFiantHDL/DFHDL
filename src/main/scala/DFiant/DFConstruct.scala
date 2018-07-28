package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFConstruct extends HasProperties with Nameable with TypeNameable with Discoverable {

}

trait DFOwnerConstruct extends DFConstruct {
  val owner : Option[DFOwnerConstruct]
  protected implicit val protChildOwnerOption : Option[this.type] = Some(this) //to be fed implicitly
  protected implicit val protChildOwner : this.type = this

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
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final private[DFiant] def newDFValGetID(dfval : DFAny) : Int = getNewID(dfvals += dfval)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}