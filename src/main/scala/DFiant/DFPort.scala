package DFiant

import DFiant.DFAny.Token
import DFiant.internals._

trait DFInterface extends HasProperties with Nameable {
  final protected type <>[DF <: DFAny, DIR <: DFDir] = DFPort.<>[DF, DIR]
  final protected type DFDir = DFPort.DFDir
  final protected type IN = DFPort.IN
  final protected type OUT = DFPort.OUT
  final protected type OPEN = DFPort.OPEN
  final protected val OPEN = DFPort.OPEN
  final protected type TOP = DFPort.TOP
  final protected val TOP = DFPort.TOP

  final lazy val ports : List[DFAny.Port[DFAny, DFDir]] =
    this.getNestedDeclaredFieldsOf[DFAny.Port[DFAny, DFDir]](classOf[DFAny.Port[DFAny, DFDir]],
      _ => true, (f, t) => if (!t.hasName) t.setAutoName(f.getName) else t)

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] = ports.filter(p => p.dir match {
    case DFPort.IN => true
    case _ => false
  }).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] = ports.filter(p => p.dir match {
    case DFPort.OUT => true
    case _ => false
  }).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])
}

object DFPort {
  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //Direction of a Port
  sealed trait DFDir
  sealed trait IN extends DFDir
  implicit object IN extends IN
  sealed trait OUT extends DFDir
  implicit object OUT extends OUT

  trait Connection[+DF <: DFAny] extends Serializable
  final case class FullyConnected[+DF <: DFAny](dfVar : DF) extends Connection[DF]
  case object OPEN extends Connection[Nothing]
  type OPEN = OPEN.type
  trait TOP
  object TOP extends TOP {
    final case class Width(width : Int) extends TOP with Connection[Nothing]
  }
}
