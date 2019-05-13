package DFiant.internals

import scala.collection.mutable.ListBuffer
import scala.collection._

trait DSLConstruct

trait DSLConfiguration {
  val foldComponents : Boolean
}

trait HasOwner {
  trait __DevHasOwner {
    implicit val owner : DSLOwnerConstruct
  }
}

trait DSLMemberConstruct extends DSLConstruct with HasProperties
  with Nameable with TypeNameable with Discoverable with HasPostConstructionOnlyDefs with HasOwner {self =>
  trait __DevDSLMemberConstruct extends __DevNameable with __DevTypeNameable with __DevDiscoverable with __DevHasOwner {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final private[internals] def getUniqueName(suggestedName : String) : String =
      ownerOption.map(o => o.getUniqueMemberName(suggestedName)).getOrElse(suggestedName)
    final lazy val fullPath : String = ownerOption.map(o => s"${o.fullName}").getOrElse("")
    final lazy val fullName : String = if (fullPath == "") name else s"$fullPath.$name"

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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final private[internals] var keepFlag : Boolean = false
    override protected def preDiscoveryRun() : Unit = {
      //Touching the name lazy val to set the final names bottom up.
      //It is important to do so to invalidate name duplicate of anonymous values.
      //For example: val result = if (someConst) new Box else new OtherBox
      //In the example we both Box and OtherBox will potentially get the name `result`,
      //but don't want to invalidate the result name for `Box` if it's in use.
      name
      //    println(s"discovered $fullName")
    }
    override protected def discoveryDependencies : List[Discoverable] = ownerOption.toList

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    lazy val ownerOption : Option[DSLOwnerConstruct] = ctx.ownerOption
    final lazy val owner : ThisOwner = ownerOption.getOrElse(unexpectedNullOwner).asInstanceOf[ThisOwner]
    def unexpectedNullOwner = throw new IllegalArgumentException("\nUnexpected null Owner")
    final lazy val nonTransparentOwner : DSLOwnerConstruct = nonTransparentOwnerOption.getOrElse(unexpectedNullOwner)
    final lazy val nonTransparentOwnerOption : Option[DSLOwnerConstruct] = ownerOption.map(o => o.nonTransparent)
    final def hasSameOwnerAs(that : DSLMemberConstruct) : Boolean =
      nonTransparentOwnerOption == that.nonTransparentOwnerOption
    final def isDownstreamMemberOf(that : DSLOwnerConstruct) : Boolean =
      (nonTransparentOwnerOption, that) match {
        case (None, _) => false
        case (Some(a), b) if a == b => true
        case (Some(a), b) => a.isDownstreamMemberOf(that)
      }
    final def isConnectedAtOwnerOf(member : DSLMemberConstruct)(
      implicit callOwner : DSLOwnerConstruct
    ) : Boolean = member.nonTransparentOwnerOption.contains(callOwner.nonTransparent)
    final def isConnectedAtEitherSide(left : DSLMemberConstruct, right : DSLMemberConstruct)(
      implicit callOwner : DSLOwnerConstruct
    ) : Boolean = isConnectedAtOwnerOf(left.nonTransparentOwner) || isConnectedAtOwnerOf(right.nonTransparentOwner)
    final protected def getID : Int = ownerOption.map(o => o.newItemGetID(self)).getOrElse(0)
    final protected lazy val id : Int = getID
    id //touch id. We only need the lazyness for initialization order
  }
  override private[DFiant] lazy val __dev : __DevDSLMemberConstruct = ???
  __dev //touch dev. We only need the lazyness for initialization order
  import __dev._

  private[DFiant] val ctx : DSLOwnerConstruct.Context[DSLOwnerConstruct, DSLConfiguration]
  type ThisOwner <: DSLOwnerConstruct
  final def keep : this.type = {
    ownerOption.foreach {
      o => o.keepMember(this)
    }
    this
  }

  override def toString: String = s"$fullName : $typeName"
}

object DSLMemberConstruct {
  implicit def fetchDev(from : DSLMemberConstruct)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
}

trait DSLOwnerConstruct extends DSLMemberConstruct {self =>
  protected[DFiant] trait __DevDSLOwnerConstruct extends __DevDSLMemberConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //the table saves the number of occurrences for each member name, to generate unique names when the scala scope
    //isn't enough to protect from reusing the same name, e.g.: loops that generate new members.
    final private[internals] var nameTable : mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
    final private[internals] def getUniqueMemberName(suggestedName : String) : String =
      nameTable.get(suggestedName) match {
        case Some(v) =>
          nameTable.update(suggestedName, v + 1)
          s"${Name.AnonStart}${suggestedName}_$v"
        case _ =>
          nameTable.update(suggestedName, 1)
          suggestedName
      }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final private def getKeepMembers : List[Discoverable] = members.filter(m => m.keepFlag)
    private[internals] def keepMember(member : DSLMemberConstruct) : Unit = {
      member.keepFlag = true
      elaborateReq = true
      keep //also keep the owner chain
    }
    override protected def discoveryDependencies : List[Discoverable] = super.discoveryDependencies ++ getKeepMembers
    final def getDiscoveredMembers : List[DSLMemberConstruct] = {
      discover()
      members.filterNot(o => o.isNotDiscovered)
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final lazy val isTop : Boolean = ownerOption.isEmpty
    lazy val nonTransparent : DSLOwnerConstruct = self
    final private[DFiant] def callSiteSameAsOwnerOf(member : DSLMemberConstruct) : Boolean =
      if (self.nonTransparent eq member.nonTransparentOwner) true
      else if (self.nonTransparentOwnerOption.isEmpty) false
      else false
    final private[internals] var mutableMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]
    @inline final def members : List[DSLMemberConstruct] = mutableMemberList.toList
    private[internals] def newItemGetID(item : DSLMemberConstruct) : Int = {
      mutableMemberList += item
      elaborateReq = true
      //    println(s"newItemGetID ${item.fullName}")
      mutableMemberList.size
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Elaboration
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private var elaborateReq : Boolean = true
    def elaborate() : Unit = if (elaborateReq) {
      members.foreach{case m : DSLOwnerConstruct => m.elaborate()} //finalize members lists of all members that can be owners
      elaborateReq = false
    }
  }
  override private[DFiant] lazy val __dev : __DevDSLOwnerConstruct = ???
  import __dev._

  protected implicit def __theOwnerToBe : DSLOwnerConstruct = this
  val __config : DSLConfiguration
}

object DSLOwnerConstruct {
  implicit def fetchDev(from : DSLOwnerConstruct)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
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
  protected[DFiant] trait __DevDSLTransparentOwnerConstruct extends __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nonTransparent : DSLOwnerConstruct = owner.nonTransparent

  }
  override private[DFiant] lazy val __dev : __DevDSLTransparentOwnerConstruct = ???
  import __dev._
}

trait DSLFoldableOwnerConstruct extends DSLOwnerConstruct {
  protected[DFiant] trait __DevDSLFoldableOwnerConstruct extends __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Folding/Unfolding
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private var foldedNameTable : mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
    private var foldedMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]

    private var folded : Boolean = false
    final def isFolded : Boolean = folded
    private[DFiant] def unfoldedRun : Unit = {}

    private lazy val firstRun : Unit = {
      foldedNameTable = mutable.HashMap.empty[String, Int] ++ nameTable
      foldedMemberList = ListBuffer.empty[DSLMemberConstruct] ++ mutableMemberList
      foldedRun
      folded = true
    }
    override def elaborate(): Unit = {
      firstRun
      if (folded != foldRequest) {
        nameTable = mutable.HashMap.empty[String, Int] ++ foldedNameTable
        mutableMemberList = ListBuffer.empty[DSLMemberConstruct] ++ foldedMemberList
        if (foldRequest) foldedRun else unfoldedRun
        folded = foldRequest
      }
      super.elaborate()
    }
  }
  override private[DFiant] lazy val __dev : __DevDSLFoldableOwnerConstruct = ???
  import __dev._
  //override foldedRun to support folded run (inject output->input dependencies and setup initialization)
  protected def foldedRun : Unit = {}

  private[DFiant] var foldRequest : Boolean = __config.foldComponents
  def fold : this.type = {foldRequest = true; this}
  def unfold : this.type = {foldRequest = false; this}

}

object DSLFoldableOwnerConstruct {
  implicit def fetchDev(from : DSLFoldableOwnerConstruct)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
}

trait DSLSelfConnectedFoldableOwnerConstruct extends DSLFoldableOwnerConstruct