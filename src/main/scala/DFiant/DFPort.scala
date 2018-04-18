package DFiant

import DFiant.DFAny.Token
import DFiant.internals._

trait DFInterface extends Taggable with Nameable {
  protected type <>[DF <: DFAny, DIR <: DFDir] = DFPort.<>[DF, DIR]
  protected type DFDir = DFPort.DFDir
  protected type IN = DFPort.IN
  protected type OUT = DFPort.OUT
  protected type OPEN = DFPort.OPEN
  protected final val OPEN = DFPort.OPEN
  protected type TOP = DFPort.TOP
  protected final val TOP = DFPort.TOP

  lazy val ports : Array[DFAny.Port[DFAny, DFDir]] = {
    getClass.getDeclaredFields
      .filter(f => f.getType.isAssignableFrom(classOf[DFAny.Port[DFAny, DFDir]]))
      .map(f => {
        f.setAccessible(true)
        f.get(this).asInstanceOf[DFAny.Port[DFAny, DFDir]]
      })
  }

  lazy val portsIn : Array[DFAny.Port[DFAny, IN]] = ports.filter(p => p.dir match {
    case DFPort.IN => true
    case _ => false
  }).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  lazy val portsOut : Array[DFAny.Port[DFAny, OUT]] = ports.filter(p => p.dir match {
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
