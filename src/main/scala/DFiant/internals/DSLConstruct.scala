package DFiant.internals

import scala.collection.mutable.ListBuffer

trait DSLConstruct {

}

trait DSLOwnableConstruct extends DSLConstruct with HasProperties with Nameable with TypeNameable with Discoverable{
  val owner : DSLOwnerConstruct
  def keep : this.type = {
    owner.keepList += this
    this
  }
}

trait DSLOwnerConstruct extends DSLOwnableConstruct {
  final protected implicit val protChildOwner : this.type = this
  private var idCnt : Int = 0
  final protected[DFiant] def getNewID(run : => Unit) : Int = {run; idCnt += 1; idCnt}

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  protected def discoveryDepenencies : List[Discoverable] = keepList.toList

  final lazy val fullName : String =
    if (owner != null) s"${owner.fullName}.$name"
    else name //Top
}