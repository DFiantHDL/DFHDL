package DFiant
import DFiant.internals._

import scala.collection.mutable.ListBuffer

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
  final private val dfvals : ListBuffer[DFAny] = ListBuffer.empty[DFAny]
  //adds the dataflow value to the list and returns its ID (starting from 1)
  final private[DFiant] def newDFValGetID(dfval : DFAny) : Int = getNewID(dfvals += dfval)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
