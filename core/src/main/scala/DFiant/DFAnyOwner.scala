package DFiant
import DFiant.BasicLib.DFBasicLib
import DFiant.internals._

trait DFAnyMember extends DSLMemberConstruct {
  protected[DFiant] trait __DevDFAnyMember extends __DevDSLMemberConstruct {
    final override lazy val ownerOption : Option[DFAnyOwner] = ctx.ownerOption
  }
  override private[DFiant] lazy val __dev : __DevDFAnyMember = ???
  import __dev._

  type ThisOwner <: DFAnyOwner
  private[DFiant] val ctx : DFAnyOwner.ContextOf[Any, DFAnyOwner]
  implicit def __theOwnerToBe : DFAnyOwner = ownerOption.get
  final implicit lazy val __config : DFAnyConfiguration = ctx.config
}

trait DFAnyOwner extends DFAnyMember with DSLOwnerConstruct {
  protected[DFiant] trait __DevDFAnyOwner extends __DevDFAnyMember with __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] def bodyCodeString : String = {
      val delim = "  "
      val noConst = getDiscoveredMembers.filterNot(e => e.isInstanceOf[DFAny.Const[_]])
      val noAnonymous = noConst.filterNot(e => e.isInstanceOf[DFAny] && e.asInstanceOf[DFAny].isAnonymous && !e.asInstanceOf[DFAny].showAnonymous)
      noAnonymous.codeString.delimRowsBy(delim)
    }
  }
  override private[DFiant] lazy val __dev : __DevDFAnyOwner = ???
  import __dev._

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
  type Context[+Owner <: DFAnyOwner] = ContextOf[Nothing, Owner]
  trait ContextWithLibOf[+T, +Owner <: DFAnyOwner] extends ContextOf[T, Owner] {
    implicit val basicLib : DFBasicLib
  }
  object ContextWithLibOf {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      evOwner : Owner,
      evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextWithLibOf[_,_]]
    ) : ContextWithLibOf[T, Owner] = new ContextWithLibOf[T, Owner] {
      val ownerOption : Option[Owner] = Some(evOwner)
      implicit val basicLib : DFBasicLib = evBasicLib
      implicit val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  type ContextWithLib[+Owner <: DFAnyOwner] = ContextWithLibOf[Nothing, Owner]

}