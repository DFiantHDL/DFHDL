package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFInterface extends HasProperties with Nameable with TypeNameable {
  final protected type <>[DF <: DFAny, DIR <: DFDir] = DFPort.<>[DF, DIR]
  final protected type DFDir = DFPort.DFDir
  final protected type IN = DFPort.IN
  final protected type OUT = DFPort.OUT
  final protected type OPEN = DFPort.OPEN
  final protected val OPEN = DFPort.OPEN
  final protected type TOP = DFPort.TOP
  final protected val TOP = DFPort.TOP

  final lazy val portNodes : List[PortNode] =
    this.getNestedDeclaredFieldsOf[DFAny.Port[DFAny, DFDir], PortNode](
      classOf[DFAny.Port[DFAny, DFDir]], (f, t) => PortNode(t, f.getName)
    )

  final protected val ports : ListBuffer[DFAny.Port[DFAny, DFDir]] = ListBuffer.empty[DFAny.Port[DFAny, DFDir]]
  final protected[DFiant] def newPortGetID(dfval : DFAny.Port[DFAny, DFDir]) : Int = {
    ports += dfval
    ports.size
  }

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] =
    ports.toList.filter(p => p.dir.isIn).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] =
    ports.toList.filter(p => p.dir.isOut).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])

  override lazy val typeName: String = {
    val cls = getClass
    val ifc = cls.getInterfaces
    if (ifc.isEmpty) cls.getSuperclass.getName else ifc.head.getName
  }

  override def toString: String = s"$name : $typeName"
}

