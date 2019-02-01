package DFiant.internals

import scala.collection.mutable.ListBuffer
import scala.collection._

trait DSLConstruct {

}

trait DSLConfiguration {
  val foldComponents : Boolean
}

trait HasOwner {
  private[DFiant] val nonTransparentOwner : DSLOwnerConstruct
  implicit val owner : DSLOwnerConstruct
}

trait DSLMemberConstruct extends DSLConstruct with HasProperties
  with Nameable with Discoverable with HasPostConstructionOnlyDefs with HasOwner {
  trait DSLMemberFields extends TypeNameable {

  }
  val __dslMemberFields : DSLMemberFields = new DSLMemberFields {}
  import __dslMemberFields._

  val ownerOption : Option[DSLOwnerConstruct]
  type ThisOwner <: DSLOwnerConstruct
  private def unexpectedNullOwner = throw new IllegalArgumentException("\nUnexpected null Owner")
  lazy val owner : ThisOwner = ownerOption.getOrElse(unexpectedNullOwner).asInstanceOf[ThisOwner]
  private[DFiant] lazy val nonTransparentOwner : DSLOwnerConstruct = nonTransparentOwnerOption.getOrElse(unexpectedNullOwner)
  private[DFiant] lazy val nonTransparentOwnerOption : Option[DSLOwnerConstruct] = ownerOption.map(o => o.nonTransparent)
  private[DFiant] def hasSameOwnerAs(that : DSLMemberConstruct) : Boolean =
    nonTransparentOwnerOption == that.nonTransparentOwnerOption
  private[DFiant] def isDownstreamMemberOf(that : DSLOwnerConstruct) : Boolean =
    (nonTransparentOwnerOption, that) match {
      case (None, _) => false
      case (Some(a), b) if a == b => true
      case (Some(a), b) => a.isDownstreamMemberOf(that)
    }
  final def keep : this.type = {
    ownerOption.foreach(o => {
      o.mutableKeepSet += this
      o.keep
    })
    this
  }
  override protected def preDiscoveryRun : Unit = {
    //Touching the name lazy val to set the final names bottom up.
    //It is important to do so to invalidate name duplicate of anonymous values.
    //For example: val result = if (someConst) new Box else new OtherBox
    //In the example we both Box and OtherBox will potentially get the name `result`,
    //but don't want to invalidate the result name for `Box` if it's in use.
    name
//    println(s"discovered $fullName")
  }

  def isConnectedAtOwnerOf(member : DSLMemberConstruct)(
    implicit callOwner : DSLOwnerConstruct
  ) : Boolean = member.nonTransparentOwnerOption.contains(callOwner.nonTransparent)
  def isConnectedAtEitherSide(left : DSLMemberConstruct, right : DSLMemberConstruct)(
    implicit callOwner : DSLOwnerConstruct
  ) : Boolean = isConnectedAtOwnerOf(left.nonTransparentOwner) || isConnectedAtOwnerOf(right.nonTransparentOwner)

  override protected def discoveryDepenencies : List[Discoverable] = ownerOption.toList
  final protected def getID : Int = ownerOption.map(o => o.newItemGetID(this)).getOrElse(0)
  val id : Int

  final lazy val fullPath : String = ownerOption.map(o => s"${o.fullName}").getOrElse("")
  final lazy val fullName : String = if (fullPath == "") name else s"$fullPath.$name"
  final private[internals] def getUniqueName(suggestedName : String) : String =
    ownerOption.map(o => o.getUniqueMemberName(suggestedName)).getOrElse(suggestedName)

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
  private[DFiant] lazy val nonTransparent : DSLOwnerConstruct = this
  val config : DSLConfiguration
  private var idCnt : Int = 0

  private[DFiant] val mutableMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]
  final lazy val memberList : List[DSLMemberConstruct] = {
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
  private val nameTable : mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
  final private[internals] def getUniqueMemberName(suggestedName : String) : String =
    nameTable.get(suggestedName) match {
      case Some(v) =>
        nameTable.update(suggestedName, v + 1)
        s"${Name.AnonStart}${suggestedName}_$v"
      case _ =>
        nameTable.update(suggestedName, 1)
        suggestedName
    }

  private[internals] val mutableKeepSet : collection.mutable.Set[Discoverable] = mutable.Set.empty[Discoverable]
  final lazy val keepList : List[Discoverable] = mutableKeepSet.toList
  override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ keepList
  final lazy val discoveredList : List[DSLMemberConstruct] = {
    discover
    memberList.filterNot(o => o.isNotDiscovered)
  }
}
object DSLOwnerConstruct {
  trait Context[+Owner <: DSLOwnerConstruct, +Config <: DSLConfiguration] {
    val ownerOption : Option[Owner]
    implicit lazy val owner : Owner =
      ownerOption.getOrElse(throw new IllegalArgumentException("\nExepcted a non-null owner, but got one"))
    implicit val config : Config
    val n : NameIt
    def getName : String = n.value
    override def toString: String = getName
  }
  trait DB[Owner, Body <: Any] {
    private case class Info(id : Int, owners : ListBuffer[Owner])
    private val db = mutable.HashMap.empty[String, mutable.HashMap[Body, Info]]
    private var dbString = ""
    private def actualTypeName(ownerTypeName : String, info : Info) : String =
      if (info.id == 0) ownerTypeName else ownerTypeName + Name.Separator + info.id
    def addOwnerBody(ownerTypeName : String, ownerBody : Body, owner : Owner) : String = {
      var newBody : Boolean = false
      val csHM = db.getOrElseUpdate(ownerTypeName, {newBody = true; mutable.HashMap.empty[Body, Info]})
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

trait DSLTransparentOwnerConstruct extends DSLOwnerConstruct {
//  override private[DFiant] lazy val nonTransparentOwner : DSLOwnerConstruct =
//    if (owner == null) null else owner.nonTransparentOwner
  override private[DFiant] lazy val nonTransparent : DSLOwnerConstruct = owner.nonTransparent

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