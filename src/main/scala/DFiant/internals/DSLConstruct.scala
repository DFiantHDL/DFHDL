package DFiant.internals

import scala.collection.mutable.ListBuffer

trait DSLConstruct {

}

trait DSLOwnableConstruct extends DSLConstruct with HasProperties with Nameable with TypeNameable with Discoverable {
  val owner : DSLOwnerConstruct
  def keep : this.type = {
    owner.keepList += this
    this
  }
  final protected def getID : Int = if (owner != null) owner.newItemGetID(this) else 0
  val id : Int
}

trait DSLOwnerConstruct extends DSLOwnableConstruct {
  final protected implicit val protChildOwner : this.type = this
  private var idCnt : Int = 0
  private val mutableOwnedList : ListBuffer[DSLOwnableConstruct] = ListBuffer.empty[DSLOwnableConstruct]
  final lazy val ownedList : List[DSLOwnableConstruct] = mutableOwnedList.toList
  final protected[internals] def newItemGetID(item : DSLOwnableConstruct) : Int = {
    mutableOwnedList += item
    idCnt += 1
    idCnt
  }

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  protected def discoveryDepenencies : List[Discoverable] = keepList.toList

  final lazy val fullName : String =
    if (owner != null) s"${owner.fullName}.$name"
    else name //Top
}