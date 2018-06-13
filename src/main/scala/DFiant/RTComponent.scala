package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit dsn : DFDesign, n : NameIt) extends DFInterface {
  protected def newGeneric() : Unit = {}

  final override val ports : ListBuffer[DFAny.Port[DFAny, DFDir]] =
    this.getNestedDeclaredFieldsOf[DFAny.Port[DFAny, DFDir]](classOf[DFAny.Port[DFAny, DFDir]],
      _ => true, (f, t) => t.setAutoName(f.getName)).to[ListBuffer]


  dsn.newRTComponent(this)
}