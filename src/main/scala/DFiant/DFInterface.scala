package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFInterface extends DFAnyOwner {
  final lazy val portNodes : List[PortNode] =
    this.getNestedDeclaredFieldsOf[DFAny.Port[DFAny, DFDir], PortNode](
      classOf[DFAny.Port[DFAny, DFDir]], (f, t) => PortNode(t, f.getName)
    )

  final protected lazy val ports : List[DFAny.Port[DFAny, DFDir]] =
    ownedList.collect{case o : DFAny => o}.filter(o => o.isPort).asInstanceOf[List[DFAny.Port[DFAny, DFDir]]]

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] =
    ports.filter(p => p.dir.isIn).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] =
    ports.filter(p => p.dir.isOut).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])

  val fullName : String
  override lazy val typeName: String = {
    val cls = getClass
    val ifc = cls.getInterfaces
    if (ifc.isEmpty) cls.getSuperclass.getName else ifc.head.getName
  }

  override def toString: String = s"$name : $typeName"
}

