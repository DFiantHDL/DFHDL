package DFiant
import DFiant.basiclib.DFBasicLib
import DFiant.internals._

trait DFAnyOwner extends DSLOwnerConstruct {
  val owner : DFAnyOwner
  final protected[DFiant] lazy val protAlmanac = newAlmanac
  final private def newAlmanac : Almanac =
    if (owner != null)
      owner.protAlmanac.fetchComponent(owner.protAlmanac.addComponent(new Almanac(name, Some(owner.protAlmanac))))
    else new Almanac(name, None)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final protected lazy val dfVals : List[DFAny.NewVar] = ownedList.collect{case o : DFAny.NewVar => o}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

object DFAnyOwner {
  trait Context[+Owner <: DFAnyOwner] extends DSLOwnerConstruct.Context[Owner]
  protected trait LowPriorityContext {
    implicit def evContext[Owner <: DFAnyOwner](implicit evContext : Context[Owner], evNameIt : NameIt)
    : Context[Owner] = new Context[Owner] {
      val owner: Owner = evContext.owner
      val n : NameIt = evNameIt
    }
  }
  object Context extends LowPriorityContext {
    implicit def ev[Owner <: DFAnyOwner](implicit evOwner : Owner, evNameIt : NameIt)
    : Context[Owner] = new Context[Owner] {
      val owner: Owner = evOwner
      val n : NameIt = evNameIt
    }
  }
  trait ContextWithBasicLib[+Owner <: DFAnyOwner] extends Context[Owner] {
    val basicLib : DFBasicLib
  }
  trait LowPriorityContextWithBasicLib {
    implicit def evContext[Owner <: DFAnyOwner](implicit evContext : Context[Owner], evBasicLib : DFBasicLib, evNameIt : NameIt)
    : ContextWithBasicLib[Owner] = new ContextWithBasicLib[Owner] {
      val owner: Owner = evContext.owner
      val basicLib : DFBasicLib = evBasicLib
      val n : NameIt = evNameIt
    }
  }
  object ContextWithBasicLib extends LowPriorityContextWithBasicLib {
    implicit def ev[Owner <: DFAnyOwner](implicit evOwner : Owner, evBasicLib : DFBasicLib, evNameIt : NameIt)
    : ContextWithBasicLib[Owner] = new ContextWithBasicLib[Owner] {
      val owner: Owner = evOwner
      val basicLib : DFBasicLib = evBasicLib
      val n : NameIt = evNameIt
    }
  }
}