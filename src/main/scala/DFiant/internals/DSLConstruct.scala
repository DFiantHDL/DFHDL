package DFiant.internals

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait DSLConstruct {

}

trait DSLConfiguration {
  val foldComponents : Boolean
}

trait DSLMemberConstruct extends DSLConstruct with HasProperties
  with Nameable with TypeNameable with Discoverable with HasPostConstructionOnlyDefs {
  val owner : DSLOwnerConstruct
  private[DFiant] def hasSameOwnerAs(that : DSLMemberConstruct) : Boolean = (owner != null) && (that.owner != null) && (owner eq that.owner)
  private[DFiant] def isDownstreamMemberOf(that : DSLOwnerConstruct) : Boolean =
    if ((owner == null) || (that == null)) false
    else if (owner eq that) true
    else owner.isDownstreamMemberOf(that)
  final def keep : this.type = {
    owner.mutableKeepList += this
    this
  }
  def isConnectedAtOwnerOf(member : DSLMemberConstruct)(
    implicit callOwner : DSLOwnerConstruct
  ) : Boolean = (member != null) && (callOwner eq member.owner)
  def isConnectedAtEitherSide(left : DSLMemberConstruct, right : DSLMemberConstruct)(
    implicit callOwner : DSLOwnerConstruct
  ) : Boolean = isConnectedAtOwnerOf(left.owner) || isConnectedAtOwnerOf(right.owner)

  protected def discoveryDepenencies : List[Discoverable] = if (owner != null) List(owner) else List()
  final protected def getID : Int = if (owner != null) owner.newItemGetID(this) else 0
  val id : Int

  final lazy val fullPath : String =
    if (owner != null) s"${owner.fullName}"
    else "" //Top

  final lazy val fullName : String = if (fullPath == "") name else s"$fullPath.$name"
  final private[internals] def getUniqueName(suggestedName : String) : String =
    if (owner != null) owner.getUniqueMemberName(suggestedName) else suggestedName

  final private def relativePath(refFullPath : String, callFullPath : String) : String = {

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

  final private def relativePath(implicit callOwner : DSLOwnerConstruct) : String =
    relativePath(fullPath, callOwner.fullName)

  final private[DFiant] def relativeName(implicit callOwner : DSLOwnerConstruct) : String = relativeName(name)(callOwner)
  final private[DFiant] def relativeName(name : String)(implicit callOwner : DSLOwnerConstruct) : String = {
    val path = relativePath(callOwner)
    if (path == "") name else s"$path.$name"
  }

  def codeString : String
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = relativeName
  override def toString: String = s"$fullName : $typeName"
}

trait DSLOwnerConstruct extends DSLMemberConstruct {
  protected implicit def theOwnerToBe : DSLOwnerConstruct = this
  val config : DSLConfiguration
  private var idCnt : Int = 0

  private def headInterfaces(c : Class[_]) : List[Class[_]] =
    c.getInterfaces.headOption match {
      case Some(i) => i :: headInterfaces(i)
      case None =>
        val s = c.getSuperclass
        if (s == null) List() else s :: headInterfaces(s)
    }
  lazy val headInterfaceNames : List[String] = headInterfaces(getClass).map(i => i.getSimpleName)

  private[internals] def fixMemberName(value : String) : String =
    if (headInterfaceNames.contains(value)) s"${Name.AnonStart}anon" else value
  private[DFiant] val mutableMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]
  private var temp : Boolean = false
  final lazy val memberList : List[DSLMemberConstruct] = {
    temp = true
    mutableMemberList.collect{case e : DSLFoldableOwnerConstruct => e.foldOrUnFoldRunOnce }
    mutableMemberList.collect{case e : DSLOwnerConstruct => e.memberList} //finalize members lists of all members that can be owners
//    println(s"memberList $fullName")
    mutableMemberList.toList
  }
  final private[internals] def newItemGetID(item : DSLMemberConstruct) : Int = {
    mutableMemberList += item
    idCnt += 1
//    println(s"newItemGetID ${item.fullName}")
    idCnt
  }
  //the table saves the number of occurrences for each member name, to generate unique names when the scala scope
  //isn't enough to protect from reusing the same name, e.g.: loops that generate new members.
  private val nameTable : HashMap[String, Int] = HashMap.empty[String, Int]
  final private[internals] def getUniqueMemberName(suggestedName : String) : String =
    nameTable.get(suggestedName) match {
      case Some(v) =>
        nameTable.update(suggestedName, v + 1)
        suggestedName + Name.Separator + v
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
    def getName : String = if (owner == null) n.value else owner.fixMemberName(n.value)
    def getOwnerName : String = n.owner
    override def toString: String = getName
  }
  trait DB[Owner, Body <: Any] {
    private case class Info(id : Int, owners : ListBuffer[Owner])
    private val db = HashMap.empty[String, HashMap[Body, Info]]
    private var dbString = ""
    private def actualTypeName(ownerTypeName : String, info : Info) : String =
      if (info.id == 0) ownerTypeName else ownerTypeName + Name.Separator + info.id
    def addOwnerBody(ownerTypeName : String, ownerBody : Body, owner : Owner) : String = {
      var newBody : Boolean = false
      val csHM = db.getOrElseUpdate(ownerTypeName, {newBody = true; HashMap.empty[Body, Info]})
      val info = csHM.getOrElseUpdate(ownerBody, {newBody = true; Info(csHM.size, ListBuffer.empty)})
      info.owners += owner
      val atn = actualTypeName(ownerTypeName, info)
      if (newBody) dbString += ownerToString(atn, ownerBody) + "\n"
      atn
    }
    def ownerToString(ownerTypeName : String, ownerBody : Body) : String
    override def toString : String = dbString
  }
}

trait DSLFoldableOwnerConstruct extends DSLOwnerConstruct {
//  private[DFiant] var foldRequest : Boolean = true
//  val fold : this.type = {foldRequest = true; this}
//  val unfold : this.type = {foldRequest = false; this}
  private[DFiant] var isFolded : Boolean = true
  private[DFiant] def unfoldedRun : Unit = isFolded = false
  //override foldedRun to support folded run (inject output->input dependencies and setup initialization)
  protected def foldedRun : Unit = unfoldedRun
  private[DFiant] lazy val foldOrUnFoldRunOnce : Unit = {
//    println(s"foldOrUnFoldRunOnce $fullName")
    if (config.foldComponents) foldedRun else unfoldedRun
  }

}

trait DSLSelfConnectedFoldableOwnerConstruct extends DSLFoldableOwnerConstruct