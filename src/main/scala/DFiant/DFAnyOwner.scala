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

  protected def bodyCodeString : String = {
    val delim = "  "
    val noConst = discoveredList.filterNot(e => e.isInstanceOf[DFAny.Const])
    delim + noConst.codeString.replaceAll("\n","\n" + delim)
  }

}

trait DFAnyConfiguration extends DSLConfiguration {
  val commentInitValues : Boolean
}
object DFAnyConfiguration {
  implicit object default extends DFAnyConfiguration {
    val commentInitValues: Boolean = false
  }
  object detailed extends DFAnyConfiguration {
    val commentInitValues: Boolean = true
  }
}

object DFAnyOwner {
  trait Context[+Owner <: DFAnyOwner] extends DSLOwnerConstruct.Context[Owner, DFAnyConfiguration]
  object Context {
    implicit def ev[Owner <: DFAnyOwner](implicit evOwner : Owner, evConfig : DFAnyConfiguration, evNameIt : NameIt)
    : Context[Owner] = new Context[Owner] {
      val owner: Owner = evOwner
      val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  trait ContextWithLib extends Context[DFBlock] {
    val basicLib : DFBasicLib
  }
  object ContextWithLib {
    implicit def ev(implicit evOwner : DFBlock, evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt)
    : ContextWithLib = new ContextWithLib {
      val owner: DFBlock = evOwner
      val basicLib : DFBasicLib = evBasicLib
      val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
}