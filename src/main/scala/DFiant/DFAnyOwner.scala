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
  trait ContextOf[T, +Owner <: DFAnyOwner] extends DSLOwnerConstruct.Context[Owner, DFAnyConfiguration]
  object ContextOf {
    implicit def ev[T, Owner <: DFAnyOwner](implicit evOwner : Owner, evConfig : DFAnyConfiguration, evNameIt : NameIt)
    : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      implicit val owner: Owner = evOwner
      implicit val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  type Context[+Owner <: DFAnyOwner] = ContextOf[Nothing, Owner]
  trait ContextWithLibOf[T, +Owner <: DFAnyOwner] extends ContextOf[T, Owner] {
    implicit val basicLib : DFBasicLib
  }
  object ContextWithLibOf {
    implicit def ev[T, Owner <: DFAnyOwner](implicit evOwner : Owner, evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt)
    : ContextWithLibOf[T, Owner] = new ContextWithLibOf[T, Owner] {
      implicit val owner: Owner = evOwner
      implicit val basicLib : DFBasicLib = evBasicLib
      implicit val config : DFAnyConfiguration = evConfig
      val n : NameIt = evNameIt
    }
  }
  type ContextWithLib[+Owner <: DFAnyOwner] = ContextWithLibOf[Nothing, Owner]

}