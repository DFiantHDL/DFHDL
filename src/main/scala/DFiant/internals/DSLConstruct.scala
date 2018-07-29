package DFiant.internals

import scala.collection.mutable.ListBuffer

trait DSLConstruct {

}

trait DSLOwnableConstruct extends DSLConstruct with HasProperties with Nameable with TypeNameable with Discoverable {
  val owner : DSLOwnerConstruct
  def keep : this.type = {
    owner.mutableKeepList += this
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

  private[internals] val mutableKeepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final lazy val keepList : List[Discoverable] = mutableKeepList.toList
  protected def discoveryDepenencies : List[Discoverable] = keepList

  final lazy val fullName : String =
    if (owner != null) s"${owner.fullName}.$name"
    else name //Top
}
object DSLOwnerConstruct {
  trait Context[+Owner <: DSLOwnerConstruct] {
    val owner : Owner
    val n : NameIt
  }
}