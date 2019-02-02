package DFiant.internals

import scala.collection.mutable.ListBuffer
import scala.collection._

trait DSLConstruct {

}

trait DSLConfiguration {
  val foldComponents : Boolean
}

trait HasOwner {
  implicit val owner : DSLOwnerConstruct
}

trait DSLMemberConstruct extends DSLConstruct with HasProperties
  with Nameable with TypeNameable with Discoverable with HasPostConstructionOnlyDefs with HasOwner {self =>
  trait __Dev extends __DevNameable with __DevTypeNameable with __DevDiscoverable {
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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
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
    final def getID : Int = ownerOption.map(o => o.newItemGetID(self)).getOrElse(0)
  }
  override private[DFiant] lazy val __dev : __Dev = new __Dev {}
  import __dev._

  val ownerOption : Option[DSLOwnerConstruct]
  type ThisOwner <: DSLOwnerConstruct
  final lazy val owner : ThisOwner = ownerOption.getOrElse(unexpectedNullOwner).asInstanceOf[ThisOwner]
  final def keep : this.type = {
    ownerOption.foreach(o => {
      o.mutableKeepSet += this
      o.keep
    })
    this
  }

  val id : Int

  def codeString : String
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = relativeName
  override def toString: String = s"$fullName : $typeName"
}

trait DSLOwnerConstruct extends DSLMemberConstruct {self =>
  trait __DevDSLOwner extends super.__Dev {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[internals] val mutableKeepSet : collection.mutable.Set[Discoverable] = mutable.Set.empty[Discoverable]
    final lazy val keepList : List[Discoverable] = mutableKeepSet.toList
    override protected def discoveryDependencies : List[Discoverable] = super.discoveryDependencies ++ keepList
    final lazy val discoveredList : List[DSLMemberConstruct] = {
      discover()
      memberList.filterNot(o => o.isNotDiscovered)
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
    final val mutableMemberList : ListBuffer[DSLMemberConstruct] = ListBuffer.empty[DSLMemberConstruct]
    final lazy val memberList : List[DSLMemberConstruct] = {
      mutableMemberList.collect{case e : DSLFoldableOwnerConstruct => e.foldOrUnFoldRunOnce }
      mutableMemberList.collect{case e : DSLOwnerConstruct => e.memberList} //finalize members lists of all members that can be owners
      //    println(s"memberList $fullName")
      mutableMemberList.toList
    }
    private var idCnt : Int = 0
    final private[internals] def newItemGetID(item : DSLMemberConstruct) : Int = {
      mutableMemberList += item
      idCnt += 1
      //    println(s"newItemGetID ${item.fullName}")
      idCnt
    }

  }
  override private[DFiant] lazy val __dev : __DevDSLOwner = new __DevDSLOwner {}
  import __dev._

  protected implicit def theOwnerToBe : DSLOwnerConstruct = this
  val config : DSLConfiguration
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
  trait __DevDSLTransparentOwner extends super.__DevDSLOwner {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nonTransparent : DSLOwnerConstruct = owner.nonTransparent

  }
  override private[DFiant] lazy val __dev : __DevDSLTransparentOwner = new __DevDSLTransparentOwner {}
  import __dev._
}

trait DSLFoldableOwnerConstruct extends DSLOwnerConstruct {
  trait __DevDSLFoldableOwner extends super.__DevDSLOwner {

  }
  override private[DFiant] lazy val __dev : __DevDSLFoldableOwner = new __DevDSLFoldableOwner {}
  import __dev._

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