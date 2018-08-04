package DFiant.internals

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait DSLConstruct {

}

trait DSLConfiguration

trait DSLMemberConstruct extends DSLConstruct with HasProperties with Nameable with TypeNameable with Discoverable {
  val owner : DSLOwnerConstruct
  def isSiblingOf(that : DSLMemberConstruct) : Boolean = (owner != null) && (that.owner != null) && (owner eq that.owner)
  def isDownstreamMemberOf(that : DSLOwnerConstruct) : Boolean =
    if (that == null) false
    else if (owner eq that) true
    else isDownstreamMemberOf(that.owner)
  def keep : this.type = {
    owner.mutableKeepList += this
    this
  }
  protected def discoveryDepenencies : List[Discoverable] = if (owner != null) List(owner) else List()
  protected def lateRun : Unit = {}
  final private[internals] lazy val lateRunOnce : Unit = lateRun
  final protected def getID : Int = if (owner != null) owner.newItemGetID(this) else 0
  val id : Int

  final lazy val fullPath : String =
    if (owner != null) s"${owner.fullName}"
    else "" //Top

  final lazy val fullName : String = if (fullPath == "") name else s"$fullPath.$name"

  private def relativePath(refFullPath : String, callFullPath : String) : String = {

    val c = callFullPath.split('.')
    val r = refFullPath.split('.')
    if (r.length < c.length) {
      val idx = r.zip(c).indexWhere(e => e._1 != e._2)
      if (idx == -1) "" else r.takeRight(c.length-idx-1).mkString(".")
    } else {
      val idx = c.zip(r).indexWhere(e => e._1 != e._2)
      if (idx == -1) r.takeRight(r.length-c.length).mkString(".") else r.takeRight(r.length-idx).mkString(".")
    }
  }

  def relativeName(implicit callOwner : DSLOwnerConstruct) : String = {
    val path = relativePath(fullPath, callOwner.fullName)
    if (path == "") name else s"$path.$name"
  }

  def codeString : String
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = relativeName
}

trait DSLOwnerConstruct extends DSLMemberConstruct {
  protected implicit def theOwnerToBe : DSLOwnerConstruct = this
  private var idCnt : Int = 0
  private val mutableMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]
  final lazy val memberList : List[DSLMemberConstruct] = {
    mutableMemberList.foreach(e => e.lateRunOnce)
    mutableMemberList.toList
  }
  final protected[internals] def newItemGetID(item : DSLMemberConstruct) : Int = {
    mutableMemberList += item
    idCnt += 1
    idCnt
  }
  //the table saves the number of occurrences for each member name, to generate unique names when the scala scope
  //isn't enough to protect from reusing the same name, e.g.: loops that generate new members.
  private val nameTable : HashMap[String, Int] = HashMap.empty[String, Int]
  final private[DFiant] def getUniqueMemberName(suggestedName : String) : String = nameTable.get(suggestedName) match {
    case Some(v) =>
      nameTable.update(suggestedName, v + 1)
      suggestedName + "$" + v
    case _ =>
      nameTable.update(suggestedName, 1)
      suggestedName
  }

  private[internals] val mutableKeepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final lazy val keepList : List[Discoverable] = mutableKeepList.toList
  override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ keepList
  final lazy val discoveredList : List[DSLMemberConstruct] = {
    discover
    memberList.filterNot(o => o.isNotDiscovered)
  }
}
object DSLOwnerConstruct {
  trait Context[+Owner <: DSLOwnerConstruct, +Config <: DSLConfiguration] {
    implicit val owner : Owner
    implicit val config : Config
    val n : NameIt
  }
}