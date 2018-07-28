package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFConstruct {
//  val id : Int
//  def codeString : String
}

trait DFOwnableConstruct extends DFConstruct with HasProperties with Nameable with TypeNameable with Discoverable{
  val owner : DFOwnerConstruct
}
trait DFOwnerConstruct extends DFOwnableConstruct  {
  final protected implicit val protChildOwner : this.type = this

  final protected[DFiant] lazy val protAlmanac = newAlmanac
  final private def newAlmanac : Almanac =
    if (owner != null)
      owner.protAlmanac.fetchComponent(owner.protAlmanac.addComponent(new Almanac(name, Some(owner.protAlmanac))))
    else new Almanac(name, None)

  private var idCnt : Int = 0
  final protected[DFiant] def getNewID(run : => Unit) : Int = {run; idCnt += 1; idCnt}

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final def keep : this.type = {
    keepList += this
    this
  }
  protected def discoveryDepenencies : List[Discoverable] = keepList.toList

  final lazy val fullName : String =
    if (owner != null) s"${owner.fullName}.$name"
    else name //Top

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final private[DFiant] def newDFValGetID(dfval : DFAny) : Int = getNewID(dfvals += dfval)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}