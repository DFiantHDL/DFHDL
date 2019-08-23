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

package DFiant
import DFiant.targetlib.TargetLib
import DFiant.internals._

import scala.annotation.tailrec
import scala.collection.immutable

trait DFAnyMember extends DSLMemberConstruct {self =>
  protected[DFiant] type ThisMember = DFAnyMember
  protected[DFiant] type ThisOwner <: DFAnyOwner

  protected[DFiant] trait __DevDFAnyMember extends __DevDSLMemberConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected[DFiant] def isNotDiscovered : Boolean = owner.discoveredMembers.contains(self)
    protected def discoveryDependencies : List[DFAnyMember] = ownerOption.toList
    final private[DFiant] def justAHack = discoveryDependencies
    final override val ownerOption : Option[DFAnyOwner] = ctx.ownerOption
    final val kept = CacheBoxRW(false)
    final var simulationKept = false
    def simulationKeep : self.type = { //force keeping this construct but only during simulation
      simulationKept = true
      self
    }
  }
  override private[DFiant] lazy val __dev : __DevDFAnyMember = ???
  import __dev._

  private[DFiant] override lazy val ctx : DFAnyOwner.ContextOf[Any, DFAnyOwner] = ???
  implicit def __theOwnerToBe : DFAnyOwner = ownerOption.get
  final implicit lazy val __config : DFAnyConfiguration = ctx.config

  final def keep : this.type = {
    ownerOption.foreach {
      o => o.__dev.keepMember(this)
    }
    this
  }
}

trait DFAnyOwner extends DFAnyMember with DSLOwnerConstruct { self =>
  protected[DFiant] trait __DevDFAnyOwner extends __DevDFAnyMember with __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    @tailrec final def discover(
      discoveredSet : immutable.HashSet[DFAnyMember],
      exploreList : List[DFAnyMember]
    ) : immutable.HashSet[DFAnyMember] = exploreList match {
      case current :: remaining =>
        if (discoveredSet.contains(current))
          discover(discoveredSet, remaining)
        else
          discover(discoveredSet + current, remaining ++ current.__dev.justAHack)
      case Nil => discoveredSet
    }

    final private lazy val keepMemberStates = CacheDerivedRO(members)(members.map(m => m.__dev.kept))
    final lazy val keepMembers = CacheDerivedRO(keepMemberStates){
      members.filter(m => m.__dev.kept || (m.__dev.simulationKept && inSimulation))
    }
    def keepMember(member : ThisMember) : Unit = {
      member.__dev.kept.set(true)
      keep //also keep the owner chain
    }
    override protected def discoveryDependencies : List[DFAnyMember] = super.discoveryDependencies ++ keepMembers
//    final private val discoveredMemberStates = CacheDerivedRO(members)(members.map(m => m.discovered))
    //    final val discoveredMembers = CacheDerivedRO(discoveredMemberStates){
    //      discover()
    //      members.filterNot(o => o.isNotDiscovered)
    //    }
    val discoveredSet : CacheBoxRO[immutable.HashSet[DFAnyMember]]
    final override protected[DFiant] def isNotDiscovered : Boolean = discoveredMembers.contains(self)

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] def bodyCodeString : String = {
      val delim = "  "
      val noAnonymous : List[ThisMember] = discoveredMembers.collect {
        case _ : DFAny.Const[_] => None
        case e : DFAny if e.isAnonymous && !e.showAnonymous => None
        case e => Some(e)
      }.flatten
      noAnonymous.codeString.delimRowsBy(delim)
    }
  }
  override private[DFiant] lazy val __dev : __DevDFAnyOwner = ???
  import __dev._

  final val discoveredMembers = CacheDerivedRO(discoveredSet, members){
    members.filter(m => discoveredSet.contains(m))
  }

  override implicit def __theOwnerToBe : DFAnyOwner = this

  protected[DFiant] lazy val inSimulation : Boolean =
    ownerOption.exists(o => o.inSimulation)

  private[DFiant] var privShowInits : Boolean = false
  final def showInits : this.type = {privShowInits = true; this}
  private[DFiant] var privShowLatencies : Boolean = false
  final def showLatencies : this.type = {privShowLatencies = true; this}
  private[DFiant] var privShowConnections : Boolean = false
  final def showConnections : this.type = {privShowConnections = true; this}
}

trait DFAnyConfiguration extends DSLConfiguration {
  val showAnonymousEntries : Boolean
  val commentInitValues : Boolean
  val commentLatencyValues : Boolean
  val commentConnection : Boolean
}
object DFAnyConfiguration {
  implicit object default extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = false
    val foldComponents : Boolean = true
    val commentInitValues: Boolean = false
    val commentLatencyValues: Boolean = false
    val commentConnection: Boolean = false
  }
  object detailed extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = true
    val foldComponents : Boolean = false
    val commentInitValues: Boolean = true
    val commentLatencyValues: Boolean = false
    val commentConnection: Boolean = false
  }
  object foldedInit extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = false
    val foldComponents : Boolean = true
    val commentInitValues: Boolean = true
    val commentLatencyValues: Boolean = false
    val commentConnection: Boolean = false
  }
  object foldedLatency extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = false
    val foldComponents : Boolean = true
    val commentInitValues: Boolean = false
    val commentLatencyValues: Boolean = true
    val commentConnection: Boolean = false
  }
  object foldedConn extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = false
    val foldComponents : Boolean = true
    val commentInitValues: Boolean = false
    val commentLatencyValues: Boolean = false
    val commentConnection: Boolean = true
  }
  object unfolded extends DFAnyConfiguration {
    val showAnonymousEntries : Boolean = false
    val foldComponents : Boolean = false
    val commentInitValues: Boolean = false
    val commentLatencyValues: Boolean = false
    val commentConnection: Boolean = false
  }
}

object DFAnyOwner {
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DSLOwnerConstruct.Context[Owner, DFAnyConfiguration]
//  trait LowPriority {
//    implicit def evContext[T, Owner <: DFAnyOwner, T2](implicit evContext : ContextOf[T2, Owner], evNameIt : NameIt)
//    : ContextOf[T, Owner] = new ContextOf[T, Owner] {
//      val ownerOption : Option[Owner] = evContext.ownerOption
//      implicit val config : DFAnyConfiguration = evContext.config
//      val n : NameIt = evNameIt
//    }
//  }
  object ContextOf {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      evOwner : Owner,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = Some(evOwner)
      implicit val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  type Context[+Owner <: DFAnyOwner] = ContextOf[Unit, Owner]
  trait ContextWithLibOf[+T, +Owner <: DFAnyOwner] extends ContextOf[T, Owner] {
    implicit val targetLib : TargetLib
  }
  object ContextWithLibOf {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      evOwner : Owner,
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextWithLibOf[_,_]]
    ) : ContextWithLibOf[T, Owner] = new ContextWithLibOf[T, Owner] {
      val ownerOption : Option[Owner] = Some(evOwner)
      implicit val targetLib : TargetLib = evBasicLib
      implicit val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  type ContextWithLib[+Owner <: DFAnyOwner] = ContextWithLibOf[Unit, Owner]

}