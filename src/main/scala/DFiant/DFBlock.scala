package DFiant

import DFiant.BasicLib.DFBasicLib
import internals._

abstract class DFBlock(implicit ctx0 : DFBlock.Context) extends DFAnyOwner with Implicits {
  val ctx = ctx0
  override type ThisOwner = DFBlock
  private[DFiant] implicit val mutableOwner : MutableOwner = new MutableOwner(this)
  final protected implicit val protInternalContext : DFBlock.InternalContext = DFBlock.InternalContext()
  override implicit def theOwnerToBe : DFBlock = mutableOwner.value
  implicit val basicLib = ctx.basicLib
  final val topDsn : DFDesign =
    ownerOption.map(o => o.asInstanceOf[DFBlock].topDsn).getOrElse(this.asInstanceOf[DFDesign])
  private[DFiant] val designDB : DFDesign.DB =
    ownerOption.map(o => o.asInstanceOf[DFBlock].designDB).getOrElse(new DFDesign.DB)
  protected val inSimulation : Boolean =
    ownerOption.exists(o => o.asInstanceOf[DFBlock].inSimulation)

  final object ifdf extends ConditionalBlock.IfNoRetVal(mutableOwner)
  final object matchdf extends ConditionalBlock.MatchNoRetVal(mutableOwner)
  def selectdf[T <: DFAny](cond : DFBool)(thenSel : T, elseSel : T) : T = ???
  def selectdf[SW, T <: DFAny](sel : DFUInt[SW], default : => Option[T] = None)(args : List[T]) : T = ???

  protected object sim {
    final val Note = Severity.Note
    final val Warning = Severity.Warning
    final val Error = Severity.Error
    def assert(cond : DFBool, msg : Message, severity : Severity = Warning) : Unit = {
      if (inSimulation) Assert(Some(cond), msg, severity)(ctx.updateOwner(theOwnerToBe))
    }
    def report(msg : Message, severity : Severity = Note) : Unit = {
      if (inSimulation) Assert(None, msg, severity)(ctx.updateOwner(theOwnerToBe))
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override private[DFiant] def nameDefault: String = ctx.getName
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //final protected def discovery : Unit = protAlmanac

  final val id = getID
}
object DFBlock {
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner] {
    self =>
    def updateOwner[Owner0 <: DFAnyOwner](owner0 : Owner0)(implicit n0 : NameIt) : ContextOf[T, Owner0] = new ContextOf[T, Owner0] {
      val ownerOption : Option[Owner0] = Some(owner0)
      implicit val basicLib: DFBasicLib = self.basicLib
      implicit val config: DFAnyConfiguration = self.config
      val n: NameIt = n0
    }
  }
  trait LowPriority {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit lp : shapeless.LowPriority, evOwner : Owner = null, evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = Option(evOwner)
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  private[DFiant] case class InternalContext()
  object ContextOf extends LowPriority {
    implicit def evContext[T, T2](implicit lp : shapeless.LowPriority, evContext : DFDesign.ContextOf[T2], external : shapeless.Refute[InternalContext])
    : ContextOf[T, DFBlock] = new ContextOf[T, DFBlock] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val basicLib : DFBasicLib = evContext.basicLib
      implicit val config : DFAnyConfiguration = evContext.config
      val n : NameIt = evContext.n
    }
  }
  type Context = ContextOf[Nothing, DFBlock]
}



class MutableOwner(var value : DFBlock)

