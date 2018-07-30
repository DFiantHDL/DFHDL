package DFiant
import DFiant.basiclib.DFBasicLib
import DFiant.internals._

trait DFAnyOwner extends DSLOwnerConstruct {
  val owner : DFAnyOwner
  final protected[DFiant] lazy val protAlmanac = fetchOrCreateAlmanac
  //create alamanac and add to owner
  final protected def fetchOrCreateAlmanac : Almanac =
    if (owner != null) owner.protAlmanac.fetchComponent(owner.protAlmanac.addComponent(createAlmanac))
    else createTopAlmanac
  final def createTopAlmanac = new Almanac(name, None)
  protected def createAlmanac = new Almanac(name, Some(owner.protAlmanac))

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFVals
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final protected lazy val dfVals : List[DFAny.NewVar] = ownedList.collect{case o : DFAny.NewVar => o}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

object DFAnyOwner {
  trait Context[+Owner <: DFAnyOwner] extends DSLOwnerConstruct.Context[Owner]
  protected trait LowPriorityContext {
    implicit def getOwner[Owner <: DFAnyOwner](implicit evContext : Context[Owner]) : Owner = evContext.owner
  }
  object Context extends LowPriorityContext {
    implicit def ev[Owner <: DFAnyOwner](implicit evOwner : Owner, evNameIt : NameIt)
    : Context[Owner] = new Context[Owner] {
      val owner: Owner = evOwner
      val n : NameIt = evNameIt
    }
  }
  trait ContextWithLib extends Context[DFBlock] {
    val basicLib : DFBasicLib
  }
  trait LowPriorityContextWithLib {
    implicit def getBasicLib(implicit evContext : ContextWithLib)
    : DFBasicLib = evContext.basicLib
  }
  object ContextWithLib extends LowPriorityContextWithLib {
    implicit def ev(implicit evOwner : DFBlock, evBasicLib : DFBasicLib, evNameIt : NameIt)
    : ContextWithLib = new ContextWithLib {
      val owner: DFBlock = evOwner
      val basicLib : DFBasicLib = evBasicLib
      val n : NameIt = evNameIt
    }
  }
}