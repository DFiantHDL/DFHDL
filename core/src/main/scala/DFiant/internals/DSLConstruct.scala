/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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

trait DSLMemberInfo extends Product with Serializable {
  val keep : Boolean
  def keep(value : Boolean) : DSLMemberInfo
  val discovered : Boolean
  def discovered(value : Boolean) : DSLMemberInfo
}
case class DSLMemberInfoCC(
  keep : Boolean,
  discovered : Boolean
) extends DSLMemberInfo {
  @inline def keep(value : Boolean) : DSLMemberInfo = copy(keep = value)
  @inline def discovered(value : Boolean) : DSLMemberInfo = copy(discovered = value)
}
object DSLMemberInfoCC {
  lazy val empty = DSLMemberInfoCC(keep = false, discovered = false)
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
    final lazy val discovered = StateDerived(dslMemberInfo)(t => t.discovered)((t, r) => t.discovered(r))
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
    final lazy val topOwner : DSLOwnerConstruct =
      ownerOption.map(o => o.topOwner).getOrElse(self.asInstanceOf[DSLOwnerConstruct])
    final lazy val dslMemberInfo =
      StateDerived(topOwner.allInfo)(t => t.getOrElse(self, DSLMemberInfoCC.empty))((t, r) => t + (self -> r))
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
  protected[DFiant] type ThisOwner <: DSLOwnerConstruct
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
    lazy val allInfo : StateFull[immutable.HashMap[DSLMemberConstruct, DSLMemberInfo]] =
      if (isTop) StateFull(immutable.HashMap()) else topOwner.allInfo

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
    def reelaborateReq() : Unit = {
      elaborateReq = true
      ownerOption.foreach(o => o.reelaborateReq())
    }
    def elaborate() : Unit = if (elaborateReq) {
      members.collect{case m : DSLOwnerConstruct => m.elaborate()} //elaborates all members that are also owners
      elaborateReq = false
    }
  }
  override private[DFiant] lazy val __dev : __DevDSLOwnerConstruct = ???
  import __dev._

  protected implicit def __theOwnerToBe : DSLOwnerConstruct = this
  val __config : DSLConfiguration
}

trait DSLContext {
  val ownerOption : Option[DSLOwnerConstruct]
  implicit lazy val owner : DSLOwnerConstruct =
    ownerOption.getOrElse(throw new IllegalArgumentException("\nExepcted a non-null owner, but got one"))
}

object DSLOwnerConstruct {
  implicit def fetchDev(from : DSLOwnerConstruct)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
  trait Context[+Owner <: DSLOwnerConstruct, +Config <: DSLConfiguration] extends DSLContext {
    val ownerOption : Option[Owner]
    override implicit lazy val owner : Owner =
      ownerOption.getOrElse(throw new IllegalArgumentException("\nExepcted a non-null owner, but got one"))
    implicit val config : Config
    val n : NameIt
    def getName : String = n.value
    override def toString: String = getName
  }
  trait DB[Owner, Body <: Any] {
    private case class Info(id : Int, order : Int, owners : ListBuffer[Owner])
    private val db = mutable.HashMap.empty[String, mutable.HashMap[Body, Info]]
//    private var dbString = ""
    private var order = 0
    private def actualTypeName(ownerTypeName : String, info : Info) : String =
      if (info.id == 0) ownerTypeName else ownerTypeName + Name.Separator + info.id
    def addOwnerBody(ownerTypeName : String, ownerBody : Body, owner : Owner) : String = {
      var newBody : Boolean = false
      val csHM = db.getOrElseUpdate(ownerTypeName, {newBody = true; mutable.HashMap.empty[Body, Info]})
      val info = csHM.getOrElseUpdate(ownerBody, {newBody = true; Info(csHM.size, order, ListBuffer.empty)})
      info.owners += owner
      val atn = actualTypeName(ownerTypeName, info)
      if (newBody) {
        order += 1
//        dbString += ownerToString(atn, ownerBody) + "\n"
      }
      atn
    }
    final lazy val namedBodies : List[(String, String)] = db.toList.flatMap(e => e._2.toList.map(f => {
      val atn = actualTypeName(e._1, f._2)
      (f._2.order, atn, ownerToString(atn, f._1)) //unsorted tuple that includes the order count
    })).sortBy(e => e._1).map(e => (e._2, e._3))
    final lazy val dbString : String = namedBodies.map(e => e._2).mkString("\n")
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

    private lazy val firstFold : Unit = {
      foldedNameTable = nameTable.clone()
      foldedMemberList = mutableMemberList.clone()
      foldedRun
      folded = true
//      foldRequest = __config.foldComponents
    }
    private[DFiant] def preFoldUnfold() : Unit = {
      nameTable = foldedNameTable.clone()
      mutableMemberList = foldedMemberList.clone()
    }
    override def elaborate(): Unit = {
      firstFold
      if (folded != foldRequest) {
        preFoldUnfold()
        if (foldRequest) foldedRun else unfoldedRun
        folded = foldRequest
      }
      super.elaborate()
    }

    private[DSLFoldableOwnerConstruct] var foldRequest : Boolean = true
  }
  override private[DFiant] lazy val __dev : __DevDSLFoldableOwnerConstruct = ???
  import __dev._
  //override foldedRun to support folded run (inject output->input dependencies and setup initialization)
  protected def foldedRun : Unit = {}

  def fold : this.type = {
    foldRequest = true
    reelaborateReq()
    this
  }
  def unfold : this.type = {
    foldRequest = false
    reelaborateReq()
    this
  }

}

object DSLFoldableOwnerConstruct {
  implicit def fetchDev(from : DSLFoldableOwnerConstruct)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
}

trait DSLSelfConnectedFoldableOwnerConstruct extends DSLFoldableOwnerConstruct